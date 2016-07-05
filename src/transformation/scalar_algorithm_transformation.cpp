/*!
   \file scalar_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Interface for all scalar transformation algorithms.
 */

#include "scalar_algorithm_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "client_client_dht_template.hpp"
#include "domain.hpp"
#include "axis.hpp"
#include "scalar.hpp"

namespace xios {

CScalarAlgorithmTransformation::CScalarAlgorithmTransformation(CScalar* scalarDestination, CScalar* scalarSource)
 : CGenericAlgorithmTransformation(),
   scalarDest_(scalarDestination),
   scalarSrc_(scalarSource), axisSrc_(0), domainSrc_(0)
{
}

CScalarAlgorithmTransformation::CScalarAlgorithmTransformation(CScalar* scalarDestination, CAxis* axisSource)
 : CGenericAlgorithmTransformation(),
   scalarDest_(scalarDestination),
   scalarSrc_(0), axisSrc_(axisSource), domainSrc_(0)
{
}

CScalarAlgorithmTransformation::CScalarAlgorithmTransformation(CScalar* scalarDestination, CDomain* domainSource)
 : CGenericAlgorithmTransformation(),
   scalarDest_(scalarDestination),
   scalarSrc_(0), axisSrc_(0), domainSrc_(domainSource)
{
}

CScalarAlgorithmTransformation::~CScalarAlgorithmTransformation()
{
}

void CScalarAlgorithmTransformation::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
}

/*!
  Compute global index of scalar on different processes
  \param [in] globalScalarIndex global index of scalar source
  \param [out] globalScalarIndexOnProc processes which contain the corresponding global index of scalar source
*/
void CScalarAlgorithmTransformation::computeExchangeGlobalIndex(const CArray<size_t,1>& globalScalarIndex,
                                                                int elementType,
                                                                CClientClientDHTInt::Index2VectorInfoTypeMap& globalScalarIndexOnProc)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  if (1 == elementType)
  {
    size_t globalIndex;
    int nIndexSize = axisSrc_->index.numElements();
    CClientClientDHTInt::Index2VectorInfoTypeMap globalIndex2ProcRank;
    globalIndex2ProcRank.rehash(std::ceil(nIndexSize/globalIndex2ProcRank.max_load_factor()));
    for (int idx = 0; idx < nIndexSize; ++idx)
    {
      globalIndex = axisSrc_->index(idx);
      globalIndex2ProcRank[globalIndex].resize(1);
      globalIndex2ProcRank[globalIndex][0] = clientRank;
    }

    CClientClientDHTInt dhtIndexProcRank(globalIndex2ProcRank, client->intraComm);
    dhtIndexProcRank.computeIndexInfoMapping(globalScalarIndex);
    globalScalarIndexOnProc = dhtIndexProcRank.getInfoIndexMap();
  }
}

}
