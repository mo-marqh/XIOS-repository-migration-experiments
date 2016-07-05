/*!
   \file dht_auto_indexing.cpp
   \author Ha NGUYEN
   \since 6 Jul 2016
   \date 6 Jul 2016

   \brief Auto assign global index across processes.
 */
#include "dht_auto_indexing.hpp"

namespace xios
{

/*!
  Constructor with initial distribution information and the corresponding index
  Each process holds a piece of hash value, which has no information (global index) associated.
The constructor tries to assign this information to each process basing on their rank.
The global index (information) are assigned across processes in the incremental manner.
The process has lower rank will have low global index.
  \param [in] hashValue Carray contains hash value
  \param [in] clientIntraComm communicator of clients
*/

CDHTAutoIndexing::CDHTAutoIndexing(const CArray<size_t,1>& hashValue,
                                   const MPI_Comm& clientIntraComm)
  : CClientClientDHTTemplate<size_t>(clientIntraComm)
{
  Index2VectorInfoTypeMap indexToVecInfoMap;
  indexToVecInfoMap.rehash(std::ceil(hashValue.size()/indexToVecInfoMap.max_load_factor()));
  CArray<size_t,1>::const_iterator it = hashValue.begin(), ite = hashValue.end();

  // Just use a dummy value to initialize
  int nbLvl = this->getNbLevel();
  for (; it != ite; ++it)
  {
    indexToVecInfoMap[*it].resize(1);
    indexToVecInfoMap[*it][0] = 0;
  }
  computeDistributedIndex(indexToVecInfoMap, clientIntraComm, nbLvl-1);

  // Find out number of index on other proc
  size_t nbIndexOnProc = index2InfoMapping_.size();
  size_t nbIndexAccum;

  MPI_Scan(&nbIndexOnProc, &nbIndexAccum, 1, MPI_UNSIGNED_LONG, MPI_SUM, clientIntraComm);

  Index2VectorInfoTypeMap::iterator itbIdx = index2InfoMapping_.begin(), itIdx,
                                    iteIdx = index2InfoMapping_.end();
  size_t idx = 0;
  size_t idxBegin = nbIndexAccum - nbIndexOnProc;
  for (itIdx = itbIdx; itIdx != iteIdx; ++itIdx)
  {
    (itIdx->second)[0] = idxBegin + idx;
    ++idx ;
  }
}

}
