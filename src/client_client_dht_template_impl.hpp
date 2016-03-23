/*!
   \file client_client_dht_template_impl.hpp
   \author Ha NGUYEN
   \since 05 Oct 2015
   \date 05 Oct 2015

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
  : index2InfoMapping_(), indexToInfoMappingLevel_()
{
  this->computeMPICommLevel(clientIntraComm, hierarLvl);
  int lvl = this->commLevel_.size() - 1;
  computeDistributedIndex(indexInfoMap, this->commLevel_[lvl], lvl);
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
  int lvl = this->commLevel_.size() - 1;
  computeIndexInfoMappingLevel(indices, this->commLevel_[lvl], lvl);
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
  int nbClient, clientRank;
  MPI_Comm_size(commLevel,&nbClient);
  MPI_Comm_rank(commLevel,&clientRank);
  std::vector<size_t> hashedIndex;
  computeHashIndex(hashedIndex, nbClient);

  size_t ssize = indices.numElements(), hashedVal;

  std::vector<size_t>::const_iterator itbClientHash = hashedIndex.begin(), itClientHash,
                                      iteClientHash = hashedIndex.end();
  std::map<int, std::vector<size_t> > client2ClientIndex;

  // Number of global index whose mapping server are on other clients
  int nbIndexToSend = 0;
  HashXIOS<size_t> hashGlobalIndex;
  for (int i = 0; i < ssize; ++i)
  {
    size_t index = indices(i);
    hashedVal  = hashGlobalIndex(index);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashedVal);
    if (iteClientHash != itClientHash)
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;
      {
        client2ClientIndex[indexClient].push_back(index);
        ++nbIndexToSend;
      }
    }
  }

  int* sendBuff = new int[nbClient];
  for (int i = 0; i < nbClient; ++i) sendBuff[i] = 0;
  std::map<int, std::vector<size_t> >::iterator itb  = client2ClientIndex.begin(), it,
                                                ite  = client2ClientIndex.end();
  for (it = itb; it != ite; ++it) sendBuff[it->first] = 1;
  int* recvBuff = new int[nbClient];
  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_SUM, commLevel);

  std::list<MPI_Request> sendIndexRequest;
  if (0 != nbIndexToSend)
      for (it = itb; it != ite; ++it)
         sendIndexToClients(it->first, it->second, commLevel, sendIndexRequest);

  int nbDemandingClient = recvBuff[clientRank], nbSendBuffInfoReceived = 0;

  // Receiving demand as well as the responds from other clients
  // The demand message contains global index; meanwhile the responds have server index information
  // Buffer to receive demand from other clients, it can be allocated or not depending whether it has demand(s)
  // There are some cases we demand duplicate index so need to determine maxium size of demanding buffer
  for (it = itb; it != ite; ++it) sendBuff[it->first] = (it->second).size();
  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_SUM, commLevel);

  unsigned long* recvBuffIndex = 0;
  int maxNbIndexDemandedFromOthers = recvBuff[clientRank];

  if (0 != maxNbIndexDemandedFromOthers)
    recvBuffIndex = new unsigned long[maxNbIndexDemandedFromOthers];

  // Buffer to receive respond from other clients, it can be allocated or not depending whether it demands other clients
//  InfoType* recvBuffInfo = 0;
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
    computeIndexInfoMappingLevel(tmpGlobalIndexOnClient, this->commLevel_[level], level);
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
        std::vector<size_t>& indexTmp = client2ClientIndex[clientSourceRank];
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
  for (int idx = 0; idx < infoToSend.size(); ++idx) delete [] infoToSend[idx];
  delete [] sendBuff;
  delete [] recvBuff;
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
  int nbClient, clientRank;
  MPI_Comm_size(commLevel,&nbClient);
  MPI_Comm_rank(commLevel,&clientRank);
  std::vector<size_t> hashedIndex;
  computeHashIndex(hashedIndex, nbClient);

  int* sendBuff = new int[nbClient];
  int* sendNbIndexBuff = new int[nbClient];
  for (int i = 0; i < nbClient; ++i)
  {
    sendBuff[i] = 0; sendNbIndexBuff[i] = 0;
  }

  // Compute size of sending and receving buffer
  std::map<int, std::vector<size_t> > client2ClientIndex;
  std::map<int, std::vector<InfoType> > client2ClientInfo;

  std::vector<size_t>::const_iterator itbClientHash = hashedIndex.begin(), itClientHash,
                                      iteClientHash = hashedIndex.end();
  typename boost::unordered_map<size_t,InfoType>::const_iterator it  = indexInfoMap.begin(),
                                                                 ite = indexInfoMap.end();
  HashXIOS<size_t> hashGlobalIndex;
  for (; it != ite; ++it)
  {
    size_t hashIndex = hashGlobalIndex(it->first);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashIndex);
    if (itClientHash != iteClientHash)
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;
      {
        sendBuff[indexClient] = 1;
        ++sendNbIndexBuff[indexClient];
        client2ClientIndex[indexClient].push_back(it->first);
        client2ClientInfo[indexClient].push_back(it->second);
      }
    }
  }

  // Calculate from how many clients each client receive message.
  int* recvBuff = new int[nbClient];
  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_SUM, commLevel);
  int recvNbClient = recvBuff[clientRank];

  // Calculate size of buffer for receiving message
  int* recvNbIndexBuff = new int[nbClient];
  MPI_Allreduce(sendNbIndexBuff, recvNbIndexBuff, nbClient, MPI_INT, MPI_SUM, commLevel);
  int recvNbIndexCount = recvNbIndexBuff[clientRank];
  unsigned long* recvIndexBuff = new unsigned long[recvNbIndexCount];
  unsigned char* recvInfoBuff = new unsigned char[recvNbIndexCount*ProcessDHTElement<InfoType>::typeSize()];

  // If a client holds information about index and the corresponding which don't belong to it,
  // it will send a message to the correct clients.
  // Contents of the message are index and its corresponding informatioin
  std::list<MPI_Request> sendRequest;
  std::map<int, std::vector<size_t> >::iterator itIndex  = client2ClientIndex.begin(),
                                                iteIndex = client2ClientIndex.end();
  for (; itIndex != iteIndex; ++itIndex)
    sendIndexToClients(itIndex->first, itIndex->second, commLevel, sendRequest);
  typename std::map<int, std::vector<InfoType> >::iterator itbInfo = client2ClientInfo.begin(), itInfo,
                                                           iteInfo = client2ClientInfo.end();

  std::vector<int> infoSizeToSend(client2ClientInfo.size(),0);
  std::vector<unsigned char*> infoToSend(client2ClientInfo.size());
  itInfo = itbInfo;
  for (int idx = 0; itInfo != iteInfo; ++itInfo, ++idx)
  {
    const std::vector<InfoType>& infoVec = itInfo->second;
    int infoVecSize = infoVec.size();
    std::vector<int> infoIndex(infoVecSize);
    for (int i = 0; i < infoVecSize; ++i)
    {
      infoIndex[i] = infoSizeToSend[idx];
      ProcessDHTElement<InfoType>::packElement(infoVec[i], NULL, infoSizeToSend[idx]);
    }

    infoToSend[idx] = new unsigned char[infoSizeToSend[idx]];
    infoSizeToSend[idx] = 0;
    for (int i = 0; i < infoVecSize; ++i)
    {
      ProcessDHTElement<InfoType>::packElement(infoVec[i], infoToSend[idx], infoSizeToSend[idx]);
    }

    sendInfoToClients(itInfo->first, infoToSend[idx], infoSizeToSend[idx], commLevel, sendRequest);
  }


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

  for (int idx = 0; idx < infoToSend.size(); ++idx) delete [] infoToSend[idx];
  delete [] sendBuff;
  delete [] sendNbIndexBuff;
  delete [] recvBuff;
  delete [] recvNbIndexBuff;
  delete [] recvIndexBuff;
  delete [] recvInfoBuff;

  // Ok, now do something recursive
  if (0 < level)
  {
    --level;
    computeDistributedIndex(indexToInfoMapping, this->commLevel_[level], level);
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
void CClientClientDHTTemplate<T,H>::sendIndexToClients(int clientDestRank, std::vector<size_t>& indices,
                                                       const MPI_Comm& clientIntraComm,
                                                       std::list<MPI_Request>& requestSendIndex)
{
  MPI_Request request;
  requestSendIndex.push_back(request);
  MPI_Isend(&(indices)[0], (indices).size(), MPI_UNSIGNED_LONG,
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

}
