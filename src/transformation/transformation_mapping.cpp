/*!
   \file transformation_mapping.cpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 18 June 2015

   \brief Take charge of communication among clients to exchange transformed data.
 */

#include "transformation_mapping.hpp"
#include <boost/unordered_map.hpp>
#include "context.hpp"
#include "context_client.hpp"
#include "distribution_client.hpp"

namespace xios {

CTransformationMapping::CTransformationMapping(CGrid* destination, CGrid* source)
  : gridSource_(source), gridDestination_(destination)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  CDistributionClient distributionClientSrc(client->clientRank, gridSource_);

  const std::vector<size_t>& globalIndexGridSrc = distributionClientSrc.getGlobalDataIndexSendToServer(); //gridSource_->getDistributionClient()->getGlobalDataIndexSendToServer();
  boost::unordered_map<size_t,int> globalIndexOfServer;
  int globalIndexSize = globalIndexGridSrc.size();
  for (int idx = 0; idx < globalIndexSize; ++idx)
  {
    globalIndexOfServer[globalIndexGridSrc[idx]] = clientRank;
  }

  gridIndexClientClientMapping_ = new CClientServerMappingDistributed(globalIndexOfServer,
                                                                      client->intraComm,
                                                                      true);
}

CTransformationMapping::CTransformationMapping(CAxis* destination, CAxis* source)
  : gridSource_(0), gridDestination_(0)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  int niSrc     = source->n.getValue();
  int ibeginSrc = source->begin.getValue();

  boost::unordered_map<size_t,int> globalIndexOfAxisSource;
  for (int idx = 0; idx < niSrc; ++idx)
  {
    globalIndexOfAxisSource[idx+ibeginSrc] = clientRank;
  }

  gridIndexClientClientMapping_ = new CClientServerMappingDistributed(globalIndexOfAxisSource,
                                                                      client->intraComm,
                                                                      true);
}

CTransformationMapping::~CTransformationMapping()
{
  if (0 != gridIndexClientClientMapping_) delete gridIndexClientClientMapping_;
}

/*!
  Suppose that we have transformations between two grids, which are represented in form of mapping between global indexes of these two grids,
this function tries to find out which clients a client needs to send and receive these global indexes to accomplish the transformations.
  The grid destination is the grid whose global indexes demande global indexes from the other grid
  Grid destination and grid source are also distributed among clients but in different manners.
  \param [in] globaIndexWeightFromDestToSource mapping representing the transformations
*/
void CTransformationMapping::computeTransformationMapping(const std::map<size_t, std::vector<std::pair<size_t,double> > >& globaIndexWeightFromDestToSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  std::map<size_t, std::vector<std::pair<size_t,double> > >::const_iterator itbMap = globaIndexWeightFromDestToSource.begin(), itMap,
                                                                            iteMap = globaIndexWeightFromDestToSource.end();

  // Not only one index on grid destination can demande two indexes from grid source
  // but an index on grid source has to be sent to two indexes of grid destination
  std::map<size_t, std::vector<std::pair<size_t,double> > > globalIndexMapFromSrcToDest;
  std::vector<std::pair<size_t,double> >::const_iterator itbVecPair, itVecPair, iteVecPair;
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    itbVecPair = (itMap->second).begin();
    iteVecPair = (itMap->second).end();
    for (itVecPair = itbVecPair; itVecPair != iteVecPair; ++itVecPair)
    {
      globalIndexMapFromSrcToDest[itVecPair->first].push_back(std::make_pair(itMap->first, itVecPair->second));
    }
  }

  // All global indexes of a client on grid destination
  CArray<size_t,1> globalIndexMap(globalIndexMapFromSrcToDest.size());
  itbMap = globalIndexMapFromSrcToDest.begin();
  iteMap = globalIndexMapFromSrcToDest.end();
  int idx = 0;
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    globalIndexMap(idx) = itMap->first;
    ++idx;
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

  std::map<int, MPI_Request> requests;

  // Inform all "source clients" about index that they need to send
  for (itMapSrc = itbMapSrc; itMapSrc != iteMapSrc; ++itMapSrc)
  {
    unsigned long* sendPtr = const_cast<unsigned long*>(&(itMapSrc->second)[0]);
    MPI_Isend(sendPtr,
              (itMapSrc->second).size(),
              MPI_UNSIGNED_LONG,
              itMapSrc->first,
              11,
              client->intraComm,
              &requests[itMapSrc->first]);
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

  std::map<int, MPI_Request>::iterator itRequest;
  for (itRequest = requests.begin(); itRequest != requests.end(); ++itRequest)
    MPI_Wait(&itRequest->second, MPI_STATUS_IGNORE);

  delete [] sendBuff;
  delete [] recvBuff;
  if (0 != buffSize) delete [] recvBuffGlobalIndex;
}

/*!
  Return (grid) global index on grid destination. This mapping contains the rank of client source (that sends info to grid destination)
and the corresponding global index to write on grid destination.
  \return global index mapping to receive on grid destination
*/
const std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >& CTransformationMapping::getGlobalIndexReceivedOnGridDestMapping() const
{
  return globalIndexReceivedOnGridDestMapping_;
}

/*!
  Return (grid) global index on grid source. This mapping contains the rank of client destination (which receives transformation info) and
the corresponding global index to send
  \return global index mapping to send on grid source
*/
const std::map<int,std::vector<size_t> >& CTransformationMapping::getGlobalIndexSendToGridDestMapping() const
{
  return globalIndexSendToGridDestMapping_;
}

}
