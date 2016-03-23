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
#include "client_client_dht_template.hpp"
#include "dht_data_types.hpp"
#include "mpi_tag.hpp"

namespace xios {

CTransformationMapping::CTransformationMapping(CGrid* destination, CGrid* source)
  : gridSource_(source), gridDestination_(destination)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  CDistributionClient distributionClientSrc(client->clientRank, gridSource_);
  const CDistributionClient::GlobalLocalDataMap& globalLocalIndexGridSrc = distributionClientSrc.getGlobalLocalDataSendToServer();
  CDistributionClient::GlobalLocalDataMap::const_iterator itIndex = globalLocalIndexGridSrc.begin(), iteIndex = globalLocalIndexGridSrc.end();

  // Mapping of global index and pair containing rank and local index
  CClientClientDHTPairIntInt::Index2InfoTypeMap globalIndexOfServer;
  PairIntInt pairIntInt;
  for (; itIndex != iteIndex; ++itIndex)
  {
    pairIntInt.first  = clientRank;
    pairIntInt.second = itIndex->second;
    globalIndexOfServer[itIndex->first] = pairIntInt;
  }

  gridIndexClientClientMapping_ = new CClientClientDHTPairIntInt(globalIndexOfServer,
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

  CClientClientDHTPairIntInt::Index2InfoTypeMap globalIndexOfAxisSource;
  PairIntInt pii;
  for (int idx = 0; idx < niSrc; ++idx)
  {
    pii.first  = clientRank;
    pii.second = idx;
    globalIndexOfAxisSource[idx+ibeginSrc] = pii; //std::make_pair(clientRank,idx);
  }

  gridIndexClientClientMapping_ = new CClientClientDHTPairIntInt(globalIndexOfAxisSource,
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
void CTransformationMapping::computeTransformationMapping(const DestinationIndexMap& globaIndexWeightFromDestToSource)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  DestinationIndexMap::const_iterator itbMap = globaIndexWeightFromDestToSource.begin(), itMap,
                                      iteMap = globaIndexWeightFromDestToSource.end();

  // Not only one index on grid destination can demande two indexes from grid source
  // but an index on grid source has to be sent to two indexes of grid destination
  DestinationIndexMap globalIndexMapFromSrcToDest;
  boost::unordered_map<size_t, int> nbGlobalIndexMapFromSrcToDest;
  std::vector<std::pair<int, std::pair<size_t,double> > >::const_iterator itbVecPair, itVecPair, iteVecPair;
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    itbVecPair = (itMap->second).begin();
    iteVecPair = (itMap->second).end();
    for (itVecPair = itbVecPair; itVecPair != iteVecPair; ++itVecPair)
    {
      ++nbGlobalIndexMapFromSrcToDest[(itVecPair->second).first];
    }
  }

  for (boost::unordered_map<size_t, int>::const_iterator it = nbGlobalIndexMapFromSrcToDest.begin();
                                                         it != nbGlobalIndexMapFromSrcToDest.end(); ++it)
  {
    globalIndexMapFromSrcToDest[it->first].reserve(it->second);
  }

  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    itbVecPair = (itMap->second).begin();
    iteVecPair = (itMap->second).end();
    for (itVecPair = itbVecPair; itVecPair != iteVecPair; ++itVecPair)
    {
      globalIndexMapFromSrcToDest[(itVecPair->second).first].push_back(std::make_pair(itVecPair->first, std::make_pair(itMap->first, (itVecPair->second).second)));
    }
  }

  // All global indexes of a client on grid destination
  CArray<size_t,1> globalIndexMap(globalIndexMapFromSrcToDest.size());
  DestinationIndexMap::const_iterator itbMapBoost = globalIndexMapFromSrcToDest.begin(), itMapBoost;
  DestinationIndexMap::const_iterator iteMapBoost = globalIndexMapFromSrcToDest.end();
  int idx = 0;
  for (itMapBoost = itbMapBoost; itMapBoost != iteMapBoost; ++itMapBoost)
  {
    globalIndexMap(idx) = itMapBoost->first;
    ++idx;
  }

  // Find out on which clients the necessary indexes of grid source are.
  gridIndexClientClientMapping_->computeIndexInfoMapping(globalIndexMap);
  const CClientClientDHTPairIntInt::Index2InfoTypeMap& globalIndexSentFromGridSource = gridIndexClientClientMapping_->getInfoIndexMap();
  CClientClientDHTPairIntInt::Index2InfoTypeMap::const_iterator itbMapSrc = globalIndexSentFromGridSource.begin(), itMapSrc,
                                                                iteMapSrc = globalIndexSentFromGridSource.end();
  std::vector<size_t>::const_iterator itbVec, itVec, iteVec;
    // Inform client about the destination to which it needs to send global indexes
  int nbClient = client->clientSize;
  std::vector<int> sendNbClientBuff(nbClient,0);
  std::vector<int> recvNbClientBuff(nbClient,0);
  std::vector<int> sendIndexBuff(nbClient,0);
  std::vector<int> recvIndexBuff(nbClient,0);
  boost::unordered_map<int,std::vector<size_t> > sendIndexMap;
  for (itMapSrc = itbMapSrc; itMapSrc != iteMapSrc; ++itMapSrc)
  {
    int sourceRank = (itMapSrc->second).first;
    (globalIndexReceivedOnGridDestMapping_[sourceRank]).push_back(globalIndexMapFromSrcToDest[itMapSrc->first]);
    sendIndexMap[sourceRank].push_back((itMapSrc->second).second);
    sendIndexMap[sourceRank].push_back(itMapSrc->first);
    sendNbClientBuff[sourceRank] = 1;
    ++sendIndexBuff[sourceRank];
  }

  MPI_Allreduce(&sendNbClientBuff[0], &recvNbClientBuff[0], nbClient, MPI_INT, MPI_SUM, client->intraComm);
  int numClientToReceive = recvNbClientBuff[client->clientRank];

  // Then specify the size of receiving buffer, because we use synch send/receive so only necessary to know maximum size
  MPI_Allreduce(&sendIndexBuff[0], &recvIndexBuff[0], nbClient, MPI_INT, MPI_MAX, client->intraComm);
  int buffSize = 2*recvIndexBuff[client->clientRank]; // we send global as well as local index
  unsigned long* recvBuffGlobalIndex;
  if (0 != buffSize) recvBuffGlobalIndex = new unsigned long [buffSize];

  std::map<int, MPI_Request> requests;

  // Inform all "source clients" about index that they need to send
  boost::unordered_map<int,std::vector<size_t> >::const_iterator itSendIndex = sendIndexMap.begin(),
                                                                 iteSendIndex= sendIndexMap.end();
  for (; itSendIndex != iteSendIndex; ++itSendIndex)
  {
    unsigned long* sendPtr = const_cast<unsigned long*>(&(itSendIndex->second)[0]);
    MPI_Isend(sendPtr,
              (itSendIndex->second).size(),
              MPI_UNSIGNED_LONG,
              (itSendIndex->first),
              MPI_TRANSFORMATION_MAPPING_INDEX,
              client->intraComm,
              &requests[(itSendIndex->first)]);
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
             MPI_TRANSFORMATION_MAPPING_INDEX,
             client->intraComm,
             &status);

    MPI_Get_count(&status, MPI_UNSIGNED_LONG, &countBuff);
    int clientDestRank = status.MPI_SOURCE;
    for (int idx = 0; idx < countBuff; idx += 2)
    {
      globalIndexSendToGridDestMapping_[clientDestRank].push_back(std::make_pair<int,size_t>(recvBuffGlobalIndex[idx], recvBuffGlobalIndex[idx+1]));
    }
    ++numClientReceived;
  }

  std::map<int, MPI_Request>::iterator itRequest;
  for (itRequest = requests.begin(); itRequest != requests.end(); ++itRequest)
    MPI_Wait(&itRequest->second, MPI_STATUS_IGNORE);

  if (0 != buffSize) delete [] recvBuffGlobalIndex;
}

/*!
  Return (grid) global index on grid destination. This mapping contains the rank of client source (that sends info to grid destination)
and the corresponding global index to write on grid destination.
  \return global index mapping to receive on grid destination
*/
const CTransformationMapping::ReceivedIndexMap& CTransformationMapping::getGlobalIndexReceivedOnGridDestMapping() const
{
  return globalIndexReceivedOnGridDestMapping_;
}

/*!
  Return (grid) global index on grid source. This mapping contains the rank of client destination (which receives transformation info) and
the corresponding global index to send
  \return global index mapping to send on grid source
*/
const CTransformationMapping::SentIndexMap& CTransformationMapping::getGlobalIndexSendToGridDestMapping() const
{
  return globalIndexSendToGridDestMapping_;
}

}
