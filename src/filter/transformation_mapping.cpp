#include "transformation_mapping.hpp"
#include <boost/unordered_map.hpp>
#include "context.hpp"
#include "context_client.hpp"

namespace xios {

CTransformationMapping::CTransformationMapping(CGrid* destination, CGrid* source)
  : gridSource_(source), gridDestination_(destination)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  const CArray<size_t,1>& globalIndexGridSrc = gridSource_->getDistributionClient()->getGlobalDataIndexSendToServer();
  boost::unordered_map<size_t,int> globalIndexOfServer;
  int globalIndexSize = globalIndexGridSrc.numElements();
  for (int idx = 0; idx < globalIndexSize; ++idx)
  {
    globalIndexOfServer[globalIndexGridSrc(idx)] = clientRank;
  }

  gridIndexClientClientMapping_ = new CClientServerMappingDistributed(globalIndexOfServer,
                                                                      client->intraComm,
                                                                      true);
}

CTransformationMapping::~CTransformationMapping()
{
  if (0 != gridIndexClientClientMapping_) delete gridIndexClientClientMapping_;
}

/*!
  Suppose that we have transformations between two grids, which are represented in form of mapping between global indexes of these two grids,
this function tries to find out which clients a client need to send and receive these global indexes to accomplish the transformations.
  The grid destination is the grid whose global indexes demande global indexes from the grid source
  Grid destination and grid source are also distributed among clients but in a different manner.
  \param [in] globaIndexMapFromDestToSource mapping representing the transformations
*/
void CTransformationMapping::computeTransformationMapping(const std::map<size_t, std::set<size_t> >& globaIndexMapFromDestToSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  int numMappingPoints = 0;
  std::map<size_t, std::set<size_t> >::const_iterator itbMap = globaIndexMapFromDestToSource.begin(), itMap,
                                                      iteMap = globaIndexMapFromDestToSource.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    numMappingPoints += (itMap->second).size();
  }

  // All global indexes of a client on grid destination
  CArray<size_t,1> globalIndexMap(numMappingPoints);
  // Not only one index on grid destination can demande two indexes from grid source
  // but an index on grid destination have to be sent to two indexes of grid destination
  std::map<size_t, std::vector<size_t> > globalIndexMapFromSrcToDest;
  std::set<size_t>::const_iterator itbSet, itSet, iteSet;
  int idx = 0;
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    itbSet = (itMap->second).begin();
    iteSet = (itMap->second).end();
    for (itSet = itbSet; itSet != iteSet; ++itSet)
    {
      globalIndexMap(idx) = *itSet;
      globalIndexMapFromSrcToDest[*itSet].push_back(itMap->first);
      ++idx;
    }
  }

  // Find out on which clients the necessary indexes of grid source are.
  gridIndexClientClientMapping_->computeServerIndexMapping(globalIndexMap);
  const std::map<int, std::vector<size_t> >& globalIndexSentFromGridSource = gridIndexClientClientMapping_->getGlobalIndexOnServer();
  std::map<int, std::vector<size_t> >::const_iterator itbMapSrc = globalIndexSentFromGridSource.begin(), itMapSrc,
                                                      iteMapSrc = globalIndexSentFromGridSource.end();
  std::vector<size_t>::const_iterator itbVec, itVec, iteVec;
  for (itMapSrc = itbMapSrc; itMapSrc != iteMapSrc; ++itMapSrc)
  {
    int sourceRank = itMapSrc->first;
    itbVec = (itMapSrc->second).begin();
    iteVec = (itMapSrc->second).end();
    for (itVec = itbVec; itVec != iteVec; ++itVec)
    {
       (globalIndexReceivedOnGridDestMapping_[sourceRank]).push_back(globalIndexMapFromSrcToDest[*itVec]);
    }
  }

  // Inform client about the destination to which it needs to send global indexes
  int nbClient = client->clientSize;
  int* sendBuff = new int[nbClient];
  int* recvBuff = new int[nbClient];
  for (int i = 0; i < nbClient; ++i) sendBuff[i] = 0;

  // First of all, inform the number of destination a client needs to send global index
  for (itMapSrc = itbMapSrc; itMapSrc != iteMapSrc; ++itMapSrc) sendBuff[itMapSrc->first] = 1;
  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_SUM, client->intraComm);
  int numClientToReceive = recvBuff[client->clientRank];

  // Then specify the size of receiving buffer, because we use synch send/receive so only necessary to know maximum size
  for (itMapSrc = itbMapSrc; itMapSrc != iteMapSrc; ++itMapSrc) sendBuff[itMapSrc->first] = (itMapSrc->second).size();
  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_MAX, client->intraComm);

  int buffSize = recvBuff[client->clientRank];
  unsigned long* recvBuffGlobalIndex;
  if (0 != buffSize) recvBuffGlobalIndex = new unsigned long [buffSize];

  // Inform all "source clients" about index that they need to send
  for (itMapSrc = itbMapSrc; itMapSrc != iteMapSrc; ++itMapSrc)
  {
    MPI_Request request;
    unsigned long* sendPtr = const_cast<unsigned long*>(&(itMapSrc->second)[0]);
    MPI_Isend(sendPtr,
              (itMapSrc->second).size(),
              MPI_UNSIGNED_LONG,
              itMapSrc->first,
              11,
              client->intraComm,
              &request);
  }

  // Now all the "source clients" try listening messages from other "destination clients"
  int numClientReceived = 0;  // number of client to which data has been already sent
  int countBuff;
  while (numClientReceived < numClientToReceive)
  {
    MPI_Status status;
    MPI_Recv(recvBuffGlobalIndex,
             buffSize,
             MPI_UNSIGNED_LONG,
             MPI_ANY_SOURCE,
             11,
             client->intraComm,
             &status);

    MPI_Get_count(&status, MPI_UNSIGNED_LONG, &countBuff);
    int clientDestRank = status.MPI_SOURCE;
    for (int idx = 0; idx < countBuff; ++idx)
    {
      globalIndexSendToGridDestMapping_[clientDestRank].push_back(recvBuffGlobalIndex[idx]);
    }
    ++numClientReceived;
  }

  delete [] sendBuff;
  delete [] recvBuff;
  if (0 != buffSize) delete [] recvBuffGlobalIndex;
}

const std::map<int,std::vector<std::vector<size_t> > >& CTransformationMapping::getGlobalIndexReceivedOnGridDestMapping() const
{
  return globalIndexReceivedOnGridDestMapping_;
}

const std::map<int,std::vector<size_t> >& CTransformationMapping::getGlobalIndexSendToGridDestMapping() const
{
  return globalIndexSendToGridDestMapping_;
}

}
