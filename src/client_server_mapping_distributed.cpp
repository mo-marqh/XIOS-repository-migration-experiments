#include "client_server_mapping_distributed.hpp"
#include <limits>
#include <boost/functional/hash.hpp>

namespace xios
{

CClientServerMappingDistributed::CClientServerMappingDistributed(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                                                 const MPI_Comm& clientIntraComm) : CClientServerMapping(), indexClientHash_()
{
  clientIntraComm_ = clientIntraComm;
  MPI_Comm_size(clientIntraComm,&(nbClient_));
  MPI_Comm_rank(clientIntraComm,&clientRank_) ;
  computeDistributedServerIndex(globalIndexOfServer, clientIntraComm);
}

CClientServerMappingDistributed::~CClientServerMappingDistributed()
{

}

void CClientServerMappingDistributed::computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient,
                                                                const CArray<int,1>& localIndexOnClient)
{
  size_t ssize = globalIndexOnClient.numElements(), hashedIndex;

  std::vector<size_t>::const_iterator itbClientHash = indexClientHash_.begin(), itClientHash,
                                      iteClientHash = indexClientHash_.end();
  std::map<int, std::vector<size_t> > client2ClientIndexGlobal;
  std::map<int, std::vector<int> > client2ClientIndexServer;
  std::map<int, std::vector<int> > clientLocalIndex;

  // Number of global index whose mapping server can be found out thanks to index-server mapping
  int nbIndexAlreadyOnClient = 0;

  // Number of global index whose mapping server are on other clients
  int nbIndexSendToOthers = 0;
  boost::hash<size_t> hashGlobalIndex;
  for (int i = 0; i < ssize; ++i)
  {
    size_t globalIndexClient = globalIndexOnClient(i);
    hashedIndex  = hashGlobalIndex(globalIndexClient);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashedIndex);
    if (iteClientHash != itClientHash)
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;

      if (clientRank_ == indexClient)
      {
        (indexGlobalOnServer_[globalIndexToServerMapping_[globalIndexClient]]).push_back(globalIndexClient);
        (localIndexSend2Server_[globalIndexToServerMapping_[globalIndexClient]]).push_back(localIndexOnClient(i));
        ++nbIndexAlreadyOnClient;
      }
      else
      {
        client2ClientIndexGlobal[indexClient].push_back(globalIndexClient);
        clientLocalIndex[indexClient].push_back(i);
        ++nbIndexSendToOthers;
      }
    }
  }

  int* sendBuff = new int[nbClient_];
  for (int i = 0; i < nbClient_; ++i) sendBuff[i] = 0;
  std::map<int, std::vector<size_t> >::iterator it  = client2ClientIndexGlobal.begin(),
                                                ite = client2ClientIndexGlobal.end();
  for (; it != ite; ++it) sendBuff[it->first] = 1;
  int* recvBuff = new int[nbClient_];
  MPI_Allreduce(sendBuff, recvBuff, nbClient_, MPI_INT, MPI_SUM, clientIntraComm_);

  std::list<MPI_Request> sendRequest;
  if (0 != nbIndexSendToOthers)
      for (it = client2ClientIndexGlobal.begin(); it != ite; ++it)
         sendIndexGlobalToClients(it->first, it->second, clientIntraComm_, sendRequest);

  // Receiving demand as well as the responds from other clients
  // The demand message contains global index; meanwhile the responds have server index information
  // Buffer to receive demand from other clients, it can be allocated or not depending whether it has demand(s)
  unsigned long* recvBuffIndexGlobal = 0;
  int maxNbIndexDemandedFromOthers = (nbIndexAlreadyOnClient >= globalIndexToServerMapping_.size())
                                   ? 0 : (globalIndexToServerMapping_.size() - nbIndexAlreadyOnClient);
  if (0 != maxNbIndexDemandedFromOthers)
    recvBuffIndexGlobal = new unsigned long[maxNbIndexDemandedFromOthers];

  // Buffer to receive respond from other clients, it can be allocated or not depending whether it demands other clients
  int* recvBuffIndexServer = 0;
  int nbIndexReceivedFromOthers = nbIndexSendToOthers;
  if (0 != nbIndexReceivedFromOthers)
    recvBuffIndexServer = new int[nbIndexReceivedFromOthers];

  resetRequestAndCount();
  std::map<int, MPI_Request>::iterator itRequest;
  std::vector<int> demandAlreadyReceived, repondAlreadyReceived;
  int nbDemandingClient = recvBuff[clientRank_], nbIndexServerReceived = 0;
  while ((0 < nbDemandingClient) || (!sendRequest.empty()) ||
         (nbIndexServerReceived < nbIndexReceivedFromOthers))
  {
    // Just check whether a client has any demand from other clients.
    // If it has, then it should send responds to these client(s)
    probeIndexGlobalMessageFromClients(recvBuffIndexGlobal, maxNbIndexDemandedFromOthers);
    if (0 < nbDemandingClient)
    {
      for (itRequest = requestRecvIndexGlobal_.begin();
           itRequest != requestRecvIndexGlobal_.end(); ++itRequest)
      {
        int flagIndexGlobal, count;
        MPI_Status statusIndexGlobal;

        MPI_Test(&(itRequest->second), &flagIndexGlobal, &statusIndexGlobal);
        if (true == flagIndexGlobal)
        {
          MPI_Get_count(&statusIndexGlobal, MPI_UNSIGNED_LONG, &count);
          int clientSourceRank = statusIndexGlobal.MPI_SOURCE;
          unsigned long* beginBuff = indexGlobalBuffBegin_[clientSourceRank];
          for (int i = 0; i < count; ++i)
          {
            client2ClientIndexServer[clientSourceRank].push_back(globalIndexToServerMapping_[*(beginBuff+i)]);
          }
          sendIndexServerToClients(clientSourceRank, client2ClientIndexServer[clientSourceRank], clientIntraComm_, sendRequest);
          --nbDemandingClient;

          demandAlreadyReceived.push_back(clientSourceRank);
        }
      }
      for (int i = 0; i< demandAlreadyReceived.size(); ++i)
        requestRecvIndexGlobal_.erase(demandAlreadyReceived[i]);
    }

    testSendRequest(sendRequest);

    // In some cases, a client need to listen respond from other clients about server information
    // Ok, with the information, a client can fill in its server-global index map.
    probeIndexServerMessageFromClients(recvBuffIndexServer, nbIndexReceivedFromOthers);
    for (itRequest = requestRecvIndexServer_.begin();
         itRequest != requestRecvIndexServer_.end();
         ++itRequest)
    {
      int flagIndexServer, count;
      MPI_Status statusIndexServer;

      MPI_Test(&(itRequest->second), &flagIndexServer, &statusIndexServer);
      if (true == flagIndexServer)
      {
        MPI_Get_count(&statusIndexServer, MPI_INT, &count);
        int clientSourceRank = statusIndexServer.MPI_SOURCE;
        int* beginBuff = indexServerBuffBegin_[clientSourceRank];
        std::vector<size_t>& globalIndexTmp = client2ClientIndexGlobal[clientSourceRank];
        std::vector<int>& localIndexTmp = clientLocalIndex[clientSourceRank];
        for (int i = 0; i < count; ++i)
        {
          (indexGlobalOnServer_[*(beginBuff+i)]).push_back(globalIndexTmp[i]);
          (localIndexSend2Server_[*(beginBuff+i)]).push_back(localIndexOnClient(localIndexTmp[i]));
        }
        nbIndexServerReceived += count;
        repondAlreadyReceived.push_back(clientSourceRank);
      }
    }

    for (int i = 0; i< repondAlreadyReceived.size(); ++i)
      requestRecvIndexServer_.erase(repondAlreadyReceived[i]);
    repondAlreadyReceived.resize(0);
  }

  if (0 != recvBuffIndexGlobal) delete recvBuffIndexGlobal;
  if (0 != recvBuffIndexServer) delete recvBuffIndexServer;
  delete [] sendBuff;
  delete [] recvBuff;
}

void CClientServerMappingDistributed::computeHashIndex()
{
  // Compute range of hash index for each client
  indexClientHash_.resize(nbClient_+1);
  size_t nbHashIndexMax = std::numeric_limits<size_t>::max();
  size_t nbHashIndex;
  indexClientHash_[0] = 0;
  for (int i = 1; i < nbClient_; ++i)
  {
    nbHashIndex = nbHashIndexMax / nbClient_;
    if (i < (nbHashIndexMax%nbClient_)) ++nbHashIndex;
    indexClientHash_[i] = indexClientHash_[i-1] + nbHashIndex;
  }
  indexClientHash_[nbClient_] = nbHashIndexMax;
}

void CClientServerMappingDistributed::computeDistributedServerIndex(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                                                    const MPI_Comm& clientIntraComm)
{
  computeHashIndex();
  int clientRank;
  MPI_Comm_rank(clientIntraComm,&clientRank);

  int* sendBuff = new int[nbClient_];
  int* sendNbIndexBuff = new int[nbClient_];
  for (int i = 0; i < nbClient_; ++i)
  {
    sendBuff[i] = 0; sendNbIndexBuff[i] = 0;
  }

  // Compute size of sending and receving buffer
  std::map<int, std::vector<size_t> > client2ClientIndexGlobal;
  std::map<int, std::vector<int> > client2ClientIndexServer;

  std::vector<size_t>::const_iterator itbClientHash = indexClientHash_.begin(), itClientHash,
                                      iteClientHash = indexClientHash_.end();
  boost::unordered_map<size_t,int>::const_iterator it  = globalIndexOfServer.begin(),
                                                   ite = globalIndexOfServer.end();
  boost::hash<size_t> hashGlobalIndex;
  for (; it != ite; ++it)
  {
    size_t hashIndex = hashGlobalIndex(it->first);
    itClientHash = std::upper_bound(itbClientHash, iteClientHash, hashIndex);
    if (itClientHash != iteClientHash)
    {
      int indexClient = std::distance(itbClientHash, itClientHash)-1;
      if (clientRank == indexClient)
      {
        globalIndexToServerMapping_.insert(std::make_pair<size_t,int>(it->first, it->second));
      }
      else
      {
        sendBuff[indexClient] = 1;
        ++sendNbIndexBuff[indexClient];
        client2ClientIndexGlobal[indexClient].push_back(it->first);
        client2ClientIndexServer[indexClient].push_back(it->second);
      }
    }
  }


//    for (boost::unordered_map<size_t,int>::const_iterator it = globalIndexToServerMapping_.begin();
//       it != globalIndexToServerMapping_.end(); ++it)
//       std::cout << " " << it->first << ":" << it->second;
//       std::cout << "First Number = " << globalIndexToServerMapping_.size() << std::endl;


  int* recvBuff = new int[nbClient_];
  MPI_Allreduce(sendBuff, recvBuff, nbClient_, MPI_INT, MPI_SUM, clientIntraComm);

  int* recvNbIndexBuff = new int[nbClient_];
  MPI_Allreduce(sendNbIndexBuff, recvNbIndexBuff, nbClient_, MPI_INT, MPI_SUM, clientIntraComm);

  MPI_Status statusIndexGlobal, statusIndexServer;
  int flag, countIndexGlobal_ = 0, countIndexServer_ = 0;


  std::map<int, MPI_Request>::iterator itRequestIndexGlobal, itRequestIndexServer;
  std::map<int, int> countBuffIndexServer, countBuffIndexGlobal;
  std::vector<int> processedList;


  bool isFinished=false;
  int recvNbIndexCount = recvNbIndexBuff[clientRank];
  int recvNbClient = recvBuff[clientRank];
  unsigned long* recvIndexGlobalBuff = new unsigned long[recvNbIndexCount];
  int* recvIndexServerBuff = new int[recvNbIndexCount];

  std::list<MPI_Request> sendRequest;
  std::map<int, std::vector<size_t> >::iterator itGlobal  = client2ClientIndexGlobal.begin(),
                                                iteGlobal = client2ClientIndexGlobal.end();
  for ( ; itGlobal != iteGlobal; ++itGlobal)
    sendIndexGlobalToClients(itGlobal->first, itGlobal->second, clientIntraComm, sendRequest);
  std::map<int, std::vector<int> >::iterator itServer  = client2ClientIndexServer.begin(),
                                             iteServer = client2ClientIndexServer.end();
  for (; itServer != iteServer; ++itServer)
    sendIndexServerToClients(itServer->first, itServer->second, clientIntraComm, sendRequest);

  resetRequestAndCount();
  while (!isFinished || (!sendRequest.empty()))
  {
    testSendRequest(sendRequest);
    probeIndexGlobalMessageFromClients(recvIndexGlobalBuff, recvNbIndexCount);

    // Processing complete request
    for (itRequestIndexGlobal = requestRecvIndexGlobal_.begin();
         itRequestIndexGlobal != requestRecvIndexGlobal_.end();
         ++itRequestIndexGlobal)
    {
      int rank = itRequestIndexGlobal->first;
      int countIndexGlobal = computeBuffCountIndexGlobal(itRequestIndexGlobal->second);
      if (0 != countIndexGlobal)
        countBuffIndexGlobal[rank] = countIndexGlobal;
    }

    probeIndexServerMessageFromClients(recvIndexServerBuff, recvNbIndexCount);
    for (itRequestIndexServer = requestRecvIndexServer_.begin();
         itRequestIndexServer != requestRecvIndexServer_.end();
         ++itRequestIndexServer)
    {
      int rank = itRequestIndexServer->first;
      int countIndexServer = computeBuffCountIndexServer(itRequestIndexServer->second);
      if (0 != countIndexServer)
        countBuffIndexServer[rank] = countIndexServer;
    }

    for (std::map<int, int>::iterator it = countBuffIndexGlobal.begin();
                                      it != countBuffIndexGlobal.end(); ++it)
    {
      int rank = it->first;
      if (countBuffIndexServer.end() != countBuffIndexServer.find(rank))
      {
        processReceivedRequest(indexGlobalBuffBegin_[rank], indexServerBuffBegin_[rank], it->second);
        processedList.push_back(rank);
        --recvNbClient;
      }

    }

    for (int i = 0; i < processedList.size(); ++i)
    {
      requestRecvIndexServer_.erase(processedList[i]);
      requestRecvIndexGlobal_.erase(processedList[i]);
      countBuffIndexGlobal.erase(processedList[i]);
      countBuffIndexServer.erase(processedList[i]);
    }

    if (0 == recvNbClient) isFinished = true;
  }

  delete [] sendBuff;
  delete [] sendNbIndexBuff;
  delete [] recvBuff;
  delete [] recvNbIndexBuff;
  delete [] recvIndexGlobalBuff;
  delete [] recvIndexServerBuff;

//    for (boost::unordered_map<size_t,int>::const_iterator it = globalIndexToServerMapping_.begin();
//       it != globalIndexToServerMapping_.end(); ++it)
//       std::cout << " " << it->first << ":" << it->second;
//       std::cout << "Number = " << globalIndexToServerMapping_.size() << std::endl;

}

void CClientServerMappingDistributed::probeIndexGlobalMessageFromClients(unsigned long* recvIndexGlobalBuff, int recvNbIndexCount)
{
  MPI_Status statusIndexGlobal;
  int flagIndexGlobal, count;

  // Probing for global index
  MPI_Iprobe(MPI_ANY_SOURCE, 15, clientIntraComm_, &flagIndexGlobal, &statusIndexGlobal);
  if ((true == flagIndexGlobal) && (countIndexGlobal_ < recvNbIndexCount))
  {
    MPI_Get_count(&statusIndexGlobal, MPI_UNSIGNED_LONG, &count);
    indexGlobalBuffBegin_.insert(std::make_pair<int, unsigned long*>(statusIndexGlobal.MPI_SOURCE, recvIndexGlobalBuff+countIndexGlobal_));
    MPI_Irecv(recvIndexGlobalBuff+countIndexGlobal_, count, MPI_UNSIGNED_LONG,
              statusIndexGlobal.MPI_SOURCE, 15, clientIntraComm_,
              &requestRecvIndexGlobal_[statusIndexGlobal.MPI_SOURCE]);
    countIndexGlobal_ += count;
  }
}

void CClientServerMappingDistributed::probeIndexServerMessageFromClients(int* recvIndexServerBuff, int recvNbIndexCount)
{
  MPI_Status statusIndexServer;
  int flagIndexServer, count;

  // Probing for server index
  MPI_Iprobe(MPI_ANY_SOURCE, 12, clientIntraComm_, &flagIndexServer, &statusIndexServer);
  if ((true == flagIndexServer) && (countIndexServer_ < recvNbIndexCount))
  {
    MPI_Get_count(&statusIndexServer, MPI_INT, &count);
    indexServerBuffBegin_.insert(std::make_pair<int, int*>(statusIndexServer.MPI_SOURCE, recvIndexServerBuff+countIndexServer_));
    MPI_Irecv(recvIndexServerBuff+countIndexServer_, count, MPI_INT,
              statusIndexServer.MPI_SOURCE, 12, clientIntraComm_,
              &requestRecvIndexServer_[statusIndexServer.MPI_SOURCE]);

    countIndexServer_ += count;
  }
}


void CClientServerMappingDistributed::sendIndexGlobalToClients(int clientDestRank, std::vector<size_t>& indexGlobal,
                                                               const MPI_Comm& clientIntraComm,
                                                               std::list<MPI_Request>& requestSendIndexGlobal)
{
  MPI_Request request;
  requestSendIndexGlobal.push_back(request);
  MPI_Isend(&(indexGlobal)[0], (indexGlobal).size(), MPI_UNSIGNED_LONG,
            clientDestRank, 15, clientIntraComm, &(requestSendIndexGlobal.back()));

//  int nbSendClient = indexGlobal.size();
//  std::map<int, std::vector<size_t> >::iterator
//                        itClient2ClientIndexGlobal  = indexGlobal.begin(),
//                        iteClient2ClientIndexGlobal = indexGlobal.end();
//
//  for (; itClient2ClientIndexGlobal != iteClient2ClientIndexGlobal;
//         ++itClient2ClientIndexGlobal)
//  {
//    MPI_Request request;
//    requestSendIndexGlobal.push_back(request);
//    MPI_Isend(&(itClient2ClientIndexGlobal->second)[0],
//              (itClient2ClientIndexGlobal->second).size(),
//              MPI_UNSIGNED_LONG,
//              itClient2ClientIndexGlobal->first,
//              15, clientIntraComm, &(requestSendIndexGlobal.back()));
//  }

}

void CClientServerMappingDistributed::sendIndexServerToClients(int clientDestRank, std::vector<int>& indexServer,
                                                               const MPI_Comm& clientIntraComm,
                                                               std::list<MPI_Request>& requestSendIndexServer)
{
  MPI_Request request;
  requestSendIndexServer.push_back(request);
  MPI_Isend(&(indexServer)[0], (indexServer).size(), MPI_INT,
            clientDestRank, 12, clientIntraComm, &(requestSendIndexServer.back()));

//  int nbSendClient = indexServer.size();
//  std::map<int, std::vector<int> >::iterator
//                        itClient2ClientIndexServer  = indexServer.begin(),
//                        iteClient2ClientIndexServer = indexServer.end();

//  for (; itClient2ClientIndexServer != iteClient2ClientIndexServer;
//         ++itClient2ClientIndexServer)
//  {
//    MPI_Request request;
//    requestSendIndexServer.push_back(request);
//    MPI_Isend(&(itClient2ClientIndexServer->second)[0],
//              (itClient2ClientIndexServer->second).size(),
//              MPI_INT,
//              itClient2ClientIndexServer->first,
//              12, clientIntraComm, &(requestSendIndexServer.back()));
//
//  }
}

void CClientServerMappingDistributed::testSendRequest(std::list<MPI_Request>& sendRequest)
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
        --sizeListRequest;
        isErased = true;
        break;
      }
    }
    if (true == isErased) sendRequest.erase(itRequest);
    ++idx;
  }
}

void CClientServerMappingDistributed::processReceivedRequest(unsigned long* buffIndexGlobal, int* buffIndexServer, int count)
{
  for (int i = 0; i < count; ++i)
    globalIndexToServerMapping_.insert(std::make_pair<size_t,int>(*(buffIndexGlobal+i),*(buffIndexServer+i)));
}

int CClientServerMappingDistributed::computeBuffCountIndexGlobal(MPI_Request& requestRecv)
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

int CClientServerMappingDistributed::computeBuffCountIndexServer(MPI_Request& requestRecv)
{
  int flag, count = 0;
  MPI_Status status;

  MPI_Test(&requestRecv, &flag, &status);
  if (true == flag)
  {
    MPI_Get_count(&status, MPI_INT, &count);
  }

  return count;
}

void CClientServerMappingDistributed::resetRequestAndCount()
{
  countIndexGlobal_ = countIndexServer_ = 0;
  requestRecvIndexGlobal_.clear();
  requestRecvIndexServer_.clear();
  indexGlobalBuffBegin_.clear();
  indexServerBuffBegin_.clear();
}

}
