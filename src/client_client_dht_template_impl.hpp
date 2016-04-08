/*!
   \file client_client_dht_template_impl.hpp
   \author Ha NGUYEN
   \since 05 Oct 2015
   \date 23 Mars 2016

   \brief Distributed hashed table implementation.
 */
#include "client_client_dht_template.hpp"
#include "utils.hpp"
#include "mpi_tag.hpp"

namespace xios
{
/*!
  Constructor with initial distribution information and the corresponding index
  Each client (process) holds a piece of information as well as the attached index, the index
will be redistributed (projected) into size_t space as long as the associated information.
  \param [in] indexInfoMap initial index and information mapping
  \param [in] clientIntraComm communicator of clients
  \param [in] hierarLvl level of hierarchy
*/
template<typename T, typename H>
CClientClientDHTTemplate<T,H>::CClientClientDHTTemplate(const boost::unordered_map<size_t,T>& indexInfoMap,
                                                        const MPI_Comm& clientIntraComm,
                                                        int hierarLvl)
  : H(clientIntraComm), index2InfoMapping_(), indexToInfoMappingLevel_()
{
  this->computeMPICommLevel();
  int nbLvl = this->getNbLevel();
  sendRank_.resize(nbLvl);
  recvRank_.resize(nbLvl);
  computeDistributedIndex(indexInfoMap, clientIntraComm, nbLvl-1);
}

template<typename T, typename H>
CClientClientDHTTemplate<T,H>::~CClientClientDHTTemplate()
{
}

/*!
  Compute mapping between indices and information corresponding to these indices
  \param [in] indices indices a proc has
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::computeIndexInfoMapping(const CArray<size_t,1>& indices)
{
  int nbLvl = this->getNbLevel();
  computeIndexInfoMappingLevel(indices, this->internalComm_, nbLvl-1);
}

/*!
    Compute mapping between indices and information corresponding to these indices
for each level of hierarchical DHT. Recursive function
   \param [in] indices indices a proc has
   \param [in] commLevel communicator of current level
   \param [in] level current level
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::computeIndexInfoMappingLevel(const CArray<size_t,1>& indices,
                                                                 const MPI_Comm& commLevel,
                                                                 int level)
{
  int clientRank;
  MPI_Comm_rank(commLevel,&clientRank);
  int groupRankBegin = this->getGroupBegin()[level];
  int nbClient = this->getNbInGroup()[level];
  std::vector<size_t> hashedIndex;
  computeHashIndex(hashedIndex, nbClient);

  size_t ssize = indices.numElements(), hashedVal;

  std::vector<size_t>::const_iterator itbClientHash = hashedIndex.begin(), itClientHash,
                                      iteClientHash = hashedIndex.end();
  std::vector<int> sendBuff(nbClient,0);
  std::vector<int> sendNbIndexBuff(nbClient,0);

  // Number of global index whose mapping server are on other clients
  int nbIndexToSend = 0;
  size_t index;
  HashXIOS<size_t> hashGlobalIndex;
  for (int i = 0; i < ssize; ++i)
  {
    index = indices(i);
    hashedVal  = hashGlobalIndex(index);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashedVal);
    int indexClient = std::distance(itbClientHash, itClientHash)-1;
    ++sendNbIndexBuff[indexClient];
  }

  std::map<int, size_t* > client2ClientIndex;
  for (int idx = 0; idx < nbClient; ++idx)
  {
    if (0 != sendNbIndexBuff[idx])
    {
      client2ClientIndex[idx+groupRankBegin] = new unsigned long [sendNbIndexBuff[idx]];
      nbIndexToSend += sendNbIndexBuff[idx];
      sendBuff[idx] = 1;
      sendNbIndexBuff[idx] = 0;
    }
  }

  for (int i = 0; i < ssize; ++i)
  {
    index = indices(i);
    hashedVal  = hashGlobalIndex(index);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashedVal);
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;
      {
        client2ClientIndex[indexClient+groupRankBegin][sendNbIndexBuff[indexClient]] = index;;
        ++sendNbIndexBuff[indexClient];
      }
    }
  }

  int recvNbClient, recvNbIndexCount;
  sendRecvRank(level, sendBuff, sendNbIndexBuff,
               recvNbClient, recvNbIndexCount);

  std::map<int, size_t* >::iterator itbIndex = client2ClientIndex.begin(), itIndex,
                                                iteIndex = client2ClientIndex.end();

  std::list<MPI_Request> sendIndexRequest;
  for (itIndex = itbIndex; itIndex != iteIndex; ++itIndex)
     sendIndexToClients(itIndex->first, (itIndex->second), sendNbIndexBuff[itIndex->first-groupRankBegin], commLevel, sendIndexRequest);

  int nbDemandingClient = recvNbClient; //recvBuff[clientRank],
  int nbSendBuffInfoReceived = 0;

  // Receiving demand as well as the responds from other clients
  // The demand message contains global index; meanwhile the responds have server index information
  // Buffer to receive demand from other clients, it can be allocated or not depending whether it has demand(s)
  // There are some cases we demand duplicate index so need to determine maxium size of demanding buffer
  unsigned long* recvBuffIndex = 0;
  int maxNbIndexDemandedFromOthers = recvNbIndexCount;
  if (0 != maxNbIndexDemandedFromOthers)
    recvBuffIndex = new unsigned long[maxNbIndexDemandedFromOthers];

  // Buffer to receive respond from other clients, it can be allocated or not depending whether it demands other clients
  unsigned char* recvBuffInfo = 0;
  int nbIndexReceivedFromOthers = nbIndexToSend;
  if (0 != nbIndexReceivedFromOthers)
    recvBuffInfo = new unsigned char[nbIndexReceivedFromOthers*ProcessDHTElement<InfoType>::typeSize()];

  std::map<int, MPI_Request>::iterator itRequest;
  std::vector<int> demandAlreadyReceived, repondAlreadyReceived;

  int countIndex = 0;  // Counting of buffer for receiving index
  std::map<int, MPI_Request> requestRecvIndex; // Request returned by MPI_IRecv function about index

  // Mapping client rank and the beginning position of receiving buffer for message of index from this client
  std::map<int, unsigned long*> indexBuffBegin;

  std::map<int,std::vector<size_t> > src2Index; // Temporary mapping contains info of demand (source and associate index) in curren level

  CArray<size_t,1> tmpGlobalIndexOnClient(maxNbIndexDemandedFromOthers);

  int k = 0;
  while ((0 < nbDemandingClient) || (!sendIndexRequest.empty()))
  {
    // Just check whether a client has any demand from other clients.
    // If it has, then it should send responds to these client(s)
    probeIndexMessageFromClients(recvBuffIndex, maxNbIndexDemandedFromOthers,
                                 countIndex, indexBuffBegin,
                                 requestRecvIndex, commLevel);
    if (0 < nbDemandingClient)
    {
      for (itRequest = requestRecvIndex.begin();
           itRequest != requestRecvIndex.end(); ++itRequest)
      {
        int flagIndexGlobal, count;
        MPI_Status statusIndexGlobal;

        MPI_Test(&(itRequest->second), &flagIndexGlobal, &statusIndexGlobal);
        if (true == flagIndexGlobal)
        {
          MPI_Get_count(&statusIndexGlobal, MPI_UNSIGNED_LONG, &count);
          int clientSourceRank = statusIndexGlobal.MPI_SOURCE;
          unsigned long* beginBuff = indexBuffBegin[clientSourceRank];
          for (int i = 0; i < count; ++i)
          {
            src2Index[clientSourceRank].push_back(*(beginBuff+i));
            tmpGlobalIndexOnClient(k) = *(beginBuff+i);
            ++k;
          }
          --nbDemandingClient;

          demandAlreadyReceived.push_back(clientSourceRank);
        }
      }
      for (int i = 0; i< demandAlreadyReceived.size(); ++i)
        requestRecvIndex.erase(demandAlreadyReceived[i]);
    }

    testSendRequest(sendIndexRequest);
  }

  if (0 < level)
  {
    --level;
    computeIndexInfoMappingLevel(tmpGlobalIndexOnClient, this->internalComm_, level);
  }
  else
    indexToInfoMappingLevel_ = index2InfoMapping_;

  std::map<int, std::vector<InfoType> > client2ClientInfo;
  std::vector<unsigned char*> infoToSend(src2Index.size());
  std::list<MPI_Request> sendInfoRequest;
  std::map<int, std::vector<size_t> >::iterator itSrc2Idx = src2Index.begin(),
                                                iteSrc2Idx = src2Index.end();
  for (int i=0; itSrc2Idx != iteSrc2Idx; ++itSrc2Idx, ++i)
  {
    int clientSourceRank = itSrc2Idx->first;
    std::vector<size_t>& srcIdx = itSrc2Idx->second;
    infoToSend[i] = new unsigned char [srcIdx.size()*ProcessDHTElement<InfoType>::typeSize()];
    int infoIndex = 0;
    for (int idx = 0; idx < srcIdx.size(); ++idx)
    {
      ProcessDHTElement<InfoType>::packElement(indexToInfoMappingLevel_[srcIdx[idx]], infoToSend[i], infoIndex);
    }
    sendInfoToClients(clientSourceRank, infoToSend[i], infoIndex, commLevel, sendInfoRequest);
  }

  boost::unordered_map<size_t,InfoType> indexToInfoMapping;
  int countInfo = 0; // Counting of buffer for receiving server index
  std::map<int, MPI_Request> requestRecvInfo;

  // Mapping client rank and the begining position of receiving buffer for message of server index from this client
  std::map<int, unsigned char*> infoBuffBegin;

  while ((!sendInfoRequest.empty()) || (nbSendBuffInfoReceived < nbIndexReceivedFromOthers))
  {
    testSendRequest(sendInfoRequest);

    // In some cases, a client need to listen respond from other clients about server information
    // Ok, with the information, a client can fill in its server-global index map.
    probeInfoMessageFromClients(recvBuffInfo, nbIndexReceivedFromOthers,
                                countInfo, infoBuffBegin,
                                requestRecvInfo, commLevel);
    for (itRequest = requestRecvInfo.begin();
         itRequest != requestRecvInfo.end();
         ++itRequest)
    {
      int flagInfo, count;
      MPI_Status statusInfo;

      MPI_Test(&(itRequest->second), &flagInfo, &statusInfo);
      if (true == flagInfo)
      {
        MPI_Get_count(&statusInfo, MPI_CHAR, &count);
        int actualCountInfo = count/infoTypeSize;
        int clientSourceRank = statusInfo.MPI_SOURCE;
        unsigned char* beginBuff = infoBuffBegin[clientSourceRank];
        size_t* indexTmp = client2ClientIndex[clientSourceRank];
        int infoIndex = 0;
        for (int i = 0; i < actualCountInfo; ++i)
        {
          ProcessDHTElement<InfoType>::unpackElement(indexToInfoMapping[indexTmp[i]], beginBuff, infoIndex);
        }
        nbSendBuffInfoReceived += actualCountInfo;
        repondAlreadyReceived.push_back(clientSourceRank);
      }
    }

    for (int i = 0; i< repondAlreadyReceived.size(); ++i)
      requestRecvInfo.erase(repondAlreadyReceived[i]);
    repondAlreadyReceived.resize(0);
  }

  indexToInfoMappingLevel_.swap(indexToInfoMapping);
  if (0 != maxNbIndexDemandedFromOthers) delete [] recvBuffIndex;
  if (0 != nbIndexReceivedFromOthers) delete [] recvBuffInfo;
  for (itIndex = itbIndex; itIndex != iteIndex; ++itIndex) delete [] itIndex->second;
  for (int idx = 0; idx < infoToSend.size(); ++idx) delete [] infoToSend[idx];
}

/*!
  Compute the hash index distribution of whole size_t space then each client will have a range of this distribution
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::computeHashIndex(std::vector<size_t>& hashedIndex, int nbClient)
{
  // Compute range of hash index for each client
  hashedIndex.resize(nbClient+1);
  size_t nbHashIndexMax = std::numeric_limits<size_t>::max();
  size_t nbHashIndex;
  hashedIndex[0] = 0;
  for (int i = 1; i < nbClient; ++i)
  {
    nbHashIndex = nbHashIndexMax / nbClient;
    if (i < (nbHashIndexMax%nbClient)) ++nbHashIndex;
    hashedIndex[i] = hashedIndex[i-1] + nbHashIndex;
  }
  hashedIndex[nbClient] = nbHashIndexMax;
}

/*!
  Compute distribution of global index for servers
  Each client already holds a piece of information and its attached index.
This information will be redistributed among processes by projecting indices into size_t space.
After the redistribution, each client holds rearranged index and its corresponding information.
  \param [in] indexInfoMap index and its corresponding info (usually server index)
  \param [in] commLevel communicator of current level
  \param [in] level current level
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::computeDistributedIndex(const boost::unordered_map<size_t,T>& indexInfoMap,
                                                            const MPI_Comm& commLevel,
                                                            int level)
{
  int clientRank;
  MPI_Comm_rank(commLevel,&clientRank);
  computeSendRecvRank(level, clientRank);

  int groupRankBegin = this->getGroupBegin()[level];
  int nbClient = this->getNbInGroup()[level];
  std::vector<size_t> hashedIndex;
  computeHashIndex(hashedIndex, nbClient);

  std::vector<int> sendBuff(nbClient,0);
  std::vector<int> sendNbIndexBuff(nbClient,0);
  std::vector<size_t>::const_iterator itbClientHash = hashedIndex.begin(), itClientHash,
                                      iteClientHash = hashedIndex.end();
  typename boost::unordered_map<size_t,InfoType>::const_iterator itb = indexInfoMap.begin(),it,
                                                                 ite = indexInfoMap.end();
  HashXIOS<size_t> hashGlobalIndex;

  // Compute size of sending and receving buffer
  for (it = itb; it != ite; ++it)
  {
    size_t hashIndex = hashGlobalIndex(it->first);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashIndex);
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;
      {
        ++sendNbIndexBuff[indexClient];
      }
    }
  }

  std::map<int, size_t*> client2ClientIndex;
  std::map<int, unsigned char*> client2ClientInfo;
  for (int idx = 0; idx < nbClient; ++idx)
  {
    if (0 != sendNbIndexBuff[idx])
    {
      client2ClientIndex[idx+groupRankBegin] = new unsigned long [sendNbIndexBuff[idx]];
      client2ClientInfo[idx+groupRankBegin] = new unsigned char [sendNbIndexBuff[idx]*ProcessDHTElement<InfoType>::typeSize()];
      sendNbIndexBuff[idx] = 0;
      sendBuff[idx] = 1;
    }
  }

  std::vector<int> sendNbInfo(nbClient,0);
  for (it = itb; it != ite; ++it)
  {
    size_t hashIndex = hashGlobalIndex(it->first);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashIndex);
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;
      {
        client2ClientIndex[indexClient + groupRankBegin][sendNbIndexBuff[indexClient]] = it->first;;
        ProcessDHTElement<InfoType>::packElement(it->second, client2ClientInfo[indexClient + groupRankBegin], sendNbInfo[indexClient]);
        ++sendNbIndexBuff[indexClient];
      }
    }
  }

  // Calculate from how many clients each client receive message.
  // Calculate size of buffer for receiving message
  int recvNbClient, recvNbIndexCount;
  sendRecvRank(level, sendBuff, sendNbIndexBuff,
               recvNbClient, recvNbIndexCount);

  // If a client holds information about index and the corresponding which don't belong to it,
  // it will send a message to the correct clients.
  // Contents of the message are index and its corresponding informatioin
  std::list<MPI_Request> sendRequest;
  std::map<int, size_t* >::iterator itbIndex = client2ClientIndex.begin(), itIndex,
                                    iteIndex = client2ClientIndex.end();
  for (itIndex = itbIndex; itIndex != iteIndex; ++itIndex)
    sendIndexToClients(itIndex->first, itIndex->second, sendNbIndexBuff[itIndex->first-groupRankBegin], commLevel, sendRequest);
  std::map<int, unsigned char*>::iterator itbInfo = client2ClientInfo.begin(), itInfo,
                                          iteInfo = client2ClientInfo.end();
  for (itInfo = itbInfo; itInfo != iteInfo; ++itInfo)
    sendInfoToClients(itInfo->first, itInfo->second, sendNbInfo[itInfo->first-groupRankBegin], commLevel, sendRequest);


  unsigned long* recvIndexBuff = new unsigned long[recvNbIndexCount];
  unsigned char* recvInfoBuff = new unsigned char[recvNbIndexCount*ProcessDHTElement<InfoType>::typeSize()];

  std::map<int, MPI_Request>::iterator itRequestIndex, itRequestInfo;
  std::map<int, int> countBuffInfo, countBuffIndex;
  std::vector<int> processedList;

  bool isFinished = (0 == recvNbClient) ? true : false;

  // Counting of buffer for receiving global index
  int countIndex = 0;

  // Counting of buffer for receiving server index
  int countInfo = 0;

  // Request returned by MPI_IRecv function about global index
  std::map<int, MPI_Request> requestRecvIndex, requestRecvInfo;

  // Mapping client rank and the beginning position of receiving buffer for message of global index from this client
  std::map<int, unsigned long*> indexBuffBegin;

  // Mapping client rank and the begining position of receiving buffer for message of server index from this client
  std::map<int, unsigned char*> infoBuffBegin;

  boost::unordered_map<size_t,InfoType> indexToInfoMapping;

  // Now each client trys to listen to demand from others.
  // If they have message, it processes: pushing global index and corresponding server to its map
  while (!isFinished || (!sendRequest.empty()))
  {
    testSendRequest(sendRequest);
    probeIndexMessageFromClients(recvIndexBuff, recvNbIndexCount,
                                 countIndex, indexBuffBegin,
                                 requestRecvIndex, commLevel);
    // Processing complete request
    for (itRequestIndex = requestRecvIndex.begin();
         itRequestIndex != requestRecvIndex.end();
         ++itRequestIndex)
    {
      int rank = itRequestIndex->first;
      int count = computeBuffCountIndex(itRequestIndex->second);
      if (0 != count)
        countBuffIndex[rank] = count;
    }

    probeInfoMessageFromClients(recvInfoBuff, recvNbIndexCount,
                                countInfo, infoBuffBegin,
                                requestRecvInfo, commLevel);
    for (itRequestInfo = requestRecvInfo.begin();
         itRequestInfo != requestRecvInfo.end();
         ++itRequestInfo)
    {
      int rank = itRequestInfo->first;
      int count = computeBuffCountInfo(itRequestInfo->second);
      if (0 != count)
        countBuffInfo[rank] = count;
    }

    for (std::map<int, int>::iterator it = countBuffIndex.begin();
                                      it != countBuffIndex.end(); ++it)
    {
      int rank = it->first;
      if ((countBuffInfo.end() != countBuffInfo.find(rank)) &&
          (countBuffIndex.end() != countBuffIndex.find(rank)))
      {
        int count = it->second;
        InfoType infoValue;
        int infoIndex = 0;
        for (int i = 0; i < count; ++i)
        {
          ProcessDHTElement<InfoType>::unpackElement(infoValue, infoBuffBegin[rank], infoIndex);
          indexToInfoMapping.insert(std::make_pair<size_t,InfoType>(*(indexBuffBegin[rank]+i),infoValue));
        }

        processedList.push_back(rank);
        --recvNbClient;
      }
    }

    for (int i = 0; i < processedList.size(); ++i)
    {
      requestRecvInfo.erase(processedList[i]);
      requestRecvIndex.erase(processedList[i]);
      countBuffIndex.erase(processedList[i]);
      countBuffInfo.erase(processedList[i]);
    }

    if (0 == recvNbClient) isFinished = true;
  }

  for (itIndex = itbIndex; itIndex != iteIndex; ++itIndex) delete [] itIndex->second;
  for (itInfo = itbInfo; itInfo != iteInfo; ++itInfo) delete [] itInfo->second;
  delete [] recvIndexBuff;
  delete [] recvInfoBuff;

  // Ok, now do something recursive
  if (0 < level)
  {
    --level;
    computeDistributedIndex(indexToInfoMapping, this->internalComm_, level);
  }
  else
    index2InfoMapping_ = indexToInfoMapping;
}

/*!
  Probe and receive message containg global index from other clients.
  Each client can send a message of global index to other clients to fulfill their maps.
Each client probes message from its queue then if the message is ready, it will be put into the receiving buffer
  \param [in] recvIndexBuff buffer dedicated for receiving global index
  \param [in] recvNbIndexCount size of the buffer
  \param [in] countIndex number of received index
  \param [in] indexBuffBegin beginning of index buffer for each source rank
  \param [in] requestRecvIndex request of receving index
  \param [in] intraComm communicator
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::probeIndexMessageFromClients(unsigned long* recvIndexBuff,
                                                                 const int recvNbIndexCount,
                                                                 int& countIndex,
                                                                 std::map<int, unsigned long*>& indexBuffBegin,
                                                                 std::map<int, MPI_Request>& requestRecvIndex,
                                                                 const MPI_Comm& intraComm)
{
  MPI_Status statusIndexGlobal;
  int flagIndexGlobal, count;

  // Probing for global index
  MPI_Iprobe(MPI_ANY_SOURCE, MPI_DHT_INDEX, intraComm, &flagIndexGlobal, &statusIndexGlobal);
  if ((true == flagIndexGlobal) && (countIndex < recvNbIndexCount))
  {
    MPI_Get_count(&statusIndexGlobal, MPI_UNSIGNED_LONG, &count);
    indexBuffBegin.insert(std::make_pair<int, unsigned long*>(statusIndexGlobal.MPI_SOURCE, recvIndexBuff+countIndex));
    MPI_Irecv(recvIndexBuff+countIndex, count, MPI_UNSIGNED_LONG,
              statusIndexGlobal.MPI_SOURCE, MPI_DHT_INDEX, intraComm,
              &requestRecvIndex[statusIndexGlobal.MPI_SOURCE]);
    countIndex += count;
  }
}

/*!
  Probe and receive message containg server index from other clients.
  Each client can send a message of server index to other clients to fulfill their maps.
Each client probes message from its queue then if the message is ready, it will be put into the receiving buffer
  \param [in] recvInfoBuff buffer dedicated for receiving server index
  \param [in] recvNbIndexCount size of the buffer
  \param [in] countInfo number of received info
  \param [in] infoBuffBegin beginning of index buffer for each source rank
  \param [in] requestRecvInfo request of receving index
  \param [in] intraComm communicator
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::probeInfoMessageFromClients(unsigned char* recvInfoBuff,
                                                                const int recvNbIndexCount,
                                                                int& countInfo,
                                                                std::map<int, unsigned char*>& infoBuffBegin,
                                                                std::map<int, MPI_Request>& requestRecvInfo,
                                                                const MPI_Comm& intraComm)
{
  MPI_Status statusInfo;
  int flagInfo, count;

  // Probing for server index
  MPI_Iprobe(MPI_ANY_SOURCE, MPI_DHT_INFO, intraComm, &flagInfo, &statusInfo);
  if ((true == flagInfo) && (countInfo < recvNbIndexCount))
  {
    MPI_Get_count(&statusInfo, MPI_CHAR, &count);
    unsigned char* beginInfoBuff = recvInfoBuff+countInfo*infoTypeSize;
    infoBuffBegin.insert(std::make_pair<int, unsigned char*>(statusInfo.MPI_SOURCE, beginInfoBuff));
    MPI_Irecv(beginInfoBuff, count, MPI_CHAR,
              statusInfo.MPI_SOURCE, MPI_DHT_INFO, intraComm,
              &requestRecvInfo[statusInfo.MPI_SOURCE]);

    countInfo += count/infoTypeSize;
  }
}

/*!
  Send message containing index to clients
  \param [in] clientDestRank rank of destination client
  \param [in] indices index to send
  \param [in] clientIntraComm communication group of client
  \param [in] requestSendIndex list of sending request
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::sendIndexToClients(int clientDestRank, size_t* indices, size_t indiceSize,
                                                       const MPI_Comm& clientIntraComm,
                                                       std::list<MPI_Request>& requestSendIndex)
{
  MPI_Request request;
  requestSendIndex.push_back(request);
  MPI_Isend(indices, indiceSize, MPI_UNSIGNED_LONG,
            clientDestRank, MPI_DHT_INDEX, clientIntraComm, &(requestSendIndex.back()));
}

/*!
  Send message containing information to clients
  \param [in] clientDestRank rank of destination client
  \param [in] info server index to send
  \param [in] clientIntraComm communication group of client
  \param [in] requestSendInfo list of sending request
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::sendInfoToClients(int clientDestRank, unsigned char* info, int infoSize,
                       const MPI_Comm& clientIntraComm, std::list<MPI_Request>& requestSendInfo)
{
  MPI_Request request;
  requestSendInfo.push_back(request);

  MPI_Isend(info, infoSize, MPI_CHAR,
            clientDestRank, MPI_DHT_INFO, clientIntraComm, &(requestSendInfo.back()));
}

/*!
  Verify status of sending request
  \param [in] sendRequest sending request to verify
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::testSendRequest(std::list<MPI_Request>& sendRequest)
{
  int flag = 0;
  MPI_Status status;
  std::list<MPI_Request>::iterator itRequest;
  int sizeListRequest = sendRequest.size();
  int idx = 0;
  while (idx < sizeListRequest)
  {
    bool isErased = false;
    for (itRequest = sendRequest.begin(); itRequest != sendRequest.end(); ++itRequest)
    {
      MPI_Test(&(*itRequest), &flag, &status);
      if (true == flag)
      {
        isErased = true;
        break;
      }
    }
    if (true == isErased) sendRequest.erase(itRequest);
    ++idx;
  }
}

/*!
  Compute size of message containing global index
  \param[in] requestRecv request of message
*/
template<typename T, typename H>
int CClientClientDHTTemplate<T,H>::computeBuffCountIndex(MPI_Request& requestRecv)
{
  int flag, count = 0;
  MPI_Status status;

  MPI_Test(&requestRecv, &flag, &status);
  if (true == flag)
  {
    MPI_Get_count(&status, MPI_UNSIGNED_LONG, &count);
  }

  return count;
}

/*!
  Compute size of message containing server index
  \param[in] requestRecv request of message
*/
template<typename T, typename H>
int CClientClientDHTTemplate<T,H>::computeBuffCountInfo(MPI_Request& requestRecv)
{
  int flag, count = 0;
  MPI_Status status;

  MPI_Test(&requestRecv, &flag, &status);
  if (true == flag)
  {
    MPI_Get_count(&status, MPI_CHAR, &count);
  }

  return (count/infoTypeSize);
}

/*!
  Compute how many processes one process needs to send to and from how many processes it will receive
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::computeSendRecvRank(int level, int rank)
{
  int groupBegin = this->getGroupBegin()[level];
  int nbInGroup  = this->getNbInGroup()[level];
  const std::vector<int>& groupParentBegin = this->getGroupParentsBegin()[level];
  const std::vector<int>& nbInGroupParents = this->getNbInGroupParents()[level];

  std::vector<size_t> hashedIndexGroup;
  computeHashIndex(hashedIndexGroup, nbInGroup);
  size_t a = hashedIndexGroup[rank-groupBegin];
  size_t b = hashedIndexGroup[rank-groupBegin+1]-1;

  int currentGroup, offset;
  size_t e,f;

  // Do a simple math [a,b) intersect [c,d)
  for (int idx = 0; idx < groupParentBegin.size(); ++idx)
  {
    std::vector<size_t> hashedIndexGroupParent;
    int nbInGroupParent = nbInGroupParents[idx];
    if (0 != nbInGroupParent)
      computeHashIndex(hashedIndexGroupParent, nbInGroupParent);
    for (int i = 0; i < nbInGroupParent; ++i)
    {
      size_t c = hashedIndexGroupParent[i];
      size_t d = hashedIndexGroupParent[i+1]-1;

    if (!((d < a) || (b <c)))
        recvRank_[level].push_back(groupParentBegin[idx]+i);
    }

    offset = rank - groupParentBegin[idx];
    if ((offset<nbInGroupParents[idx]) && (0 <= offset))
    {
      e = hashedIndexGroupParent[offset];
      f = hashedIndexGroupParent[offset+1]-1;
    }
  }

  std::vector<size_t>::const_iterator itbHashGroup = hashedIndexGroup.begin(), itHashGroup,
                                      iteHashGroup = hashedIndexGroup.end();
  itHashGroup = std::lower_bound(itbHashGroup, iteHashGroup, e+1);
  int begin = std::distance(itbHashGroup, itHashGroup)-1;
  itHashGroup = std::upper_bound(itbHashGroup, iteHashGroup, f);
  int end = std::distance(itbHashGroup, itHashGroup) -1;
  sendRank_[level].resize(end-begin+1);
  for (int idx = 0; idx < sendRank_[level].size(); ++idx) sendRank_[level][idx] = idx + groupBegin + begin;
}

/*!
  Send and receive number of process each process need to listen to as well as number
  of index it will receive
*/
template<typename T, typename H>
void CClientClientDHTTemplate<T,H>::sendRecvRank(int level,
                                                 const std::vector<int>& sendNbRank, const std::vector<int>& sendNbElements,
                                                 int& recvNbRank, int& recvNbElements)
{
  int groupBegin = this->getGroupBegin()[level];

  int offSet = 0;
  std::vector<int>& sendRank = sendRank_[level];
  std::vector<int>& recvRank = recvRank_[level];
  int sendBuffSize = sendRank.size();
  int* sendBuff = new int [sendBuffSize*2];
  std::vector<MPI_Request> request(sendBuffSize);
  std::vector<MPI_Status> requestStatus(sendBuffSize);
  int recvBuffSize = recvRank.size();
  int* recvBuff = new int [2];

  for (int idx = 0; idx < sendBuffSize; ++idx)
  {
    offSet = sendRank[idx]-groupBegin;
    sendBuff[idx*2] = sendNbRank[offSet];
    sendBuff[idx*2+1] = sendNbElements[offSet];
  }

  for (int idx = 0; idx < sendBuffSize; ++idx)
  {
    MPI_Isend(&sendBuff[idx*2], 2, MPI_INT,
              sendRank[idx], MPI_DHT_INDEX_1, this->internalComm_, &request[idx]);
  }

  MPI_Status status;
  int nbRecvRank = 0, nbRecvElements = 0;
  for (int idx = 0; idx < recvBuffSize; ++idx)
  {
    MPI_Recv(recvBuff, 2, MPI_INT,
             recvRank[idx], MPI_DHT_INDEX_1, this->internalComm_, &status);
    nbRecvRank += *(recvBuff);
    nbRecvElements += *(recvBuff+1);
  }

  MPI_Waitall(sendBuffSize, &request[0], &requestStatus[0]);

  recvNbRank = nbRecvRank;
  recvNbElements = nbRecvElements;

  delete [] sendBuff;
  delete [] recvBuff;
}

}
