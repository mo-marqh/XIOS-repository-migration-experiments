/*!
   \file domain_algorithm_generate_rectilinear.cpp
   \author Ha NGUYEN
   \since 31 Aug 2015
   \date 31 Aug 2015

   \brief Algorithm for automatic generation of rectilinear domain.
 */
#include "domain_algorithm_generate_rectilinear.hpp"
#include "grid.hpp"
#include "domain.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "generate_rectilinear_domain.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {


shared_ptr<CGenericAlgorithmTransformation> CDomainAlgorithmGenerateRectilinear::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                                     CTransformation<CDomain>* transformation,
                                                                     int elementPositionInGrid,
                                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition)
TRY
{
  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

  CGenerateRectilinearDomain* transform = dynamic_cast<CGenerateRectilinearDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2DomainPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return make_shared<CDomainAlgorithmGenerateRectilinear>(isSource, domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], gridDst, gridSrc, transform);
}
CATCH

bool CDomainAlgorithmGenerateRectilinear::dummyRegistered_ = CDomainAlgorithmGenerateRectilinear::registerTrans();

bool CDomainAlgorithmGenerateRectilinear::registerTrans()
TRY
{
  return CGridTransformationFactory<CDomain>::registerTransformation(TRANS_GENERATE_RECTILINEAR_DOMAIN, create);
}
CATCH



CDomainAlgorithmGenerateRectilinear::CDomainAlgorithmGenerateRectilinear(bool isSource, CDomain* domainDestination, CDomain* domainSource,
                                                                         CGrid* gridDest, CGrid* gridSource,
                                                                         CGenerateRectilinearDomain* genRectDomain)
: CAlgorithmTransformationNoDataModification(isSource), nbDomainDistributedPart_(0), domainDest_(domainDestination)
TRY
{
  genRectDomain->checkValid(domainDestination);
  if (0 != gridSource) computeDistributionGridSource(gridSource);
  else
  {
    computeDistributionGridDestination(gridDest);
  }
  fillInAttributesDomainDestination();
  domainDestination->checkAttributes() ;
}
CATCH

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmGenerateRectilinear::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
 /* Nothing to do */
}

/*!
  Calculate the number of distributed parts on domain source
*/
void CDomainAlgorithmGenerateRectilinear::computeDistributionGridSource(CGrid* gridSrc)
TRY
{
  CContext* context = CContext::getCurrent();
  int clientSize = context->intraCommSize_ ;
  int clientRank = context->intraCommRank_ ;
  
  std::vector<CDomain*> domListSrcP = gridSrc->getDomains();
  std::vector<CAxis*> axisListSrcP = gridSrc->getAxis();

  for (int i = 0; i < domListSrcP.size(); ++i) // support we have only domain, more than one, for now, dont know how to process
  {
    // First, find (roundly) distribution of associated axis (if any)
    if (axisListSrcP.empty()) nbDomainDistributedPart_ = clientSize;
    else
    {
      gridSrc->solveAxisRef(false);
      int nbAxis = axisListSrcP.size();
      std::vector<int> nbLocalAxis(nbAxis, 0);
      for (int j = 0; j < nbAxis; ++j)
      {
        std::vector<int> globalAxisIndex(axisListSrcP[j]->n);
        for (int idx = 0; idx < axisListSrcP[j]->n; ++idx)
          globalAxisIndex[idx] = axisListSrcP[j]->begin + idx;
        HashXIOS<int> hashFunc;
        StdSize hashValue = hashFunc.hashVec(globalAxisIndex);
        std::vector<StdSize> recvBuff(clientSize);
        MPI_Gather(&hashValue, 1, MPI_UNSIGNED_LONG,
                   &recvBuff[0], 1, MPI_UNSIGNED_LONG,
                   0,
                   context->intraComm_);
        if (0 == clientRank)
        {
          std::set<StdSize> setTmp;
          for (int k = 0; k < recvBuff.size(); ++k)
          {
            if (setTmp.end() == setTmp.find(recvBuff[k]))
            {
              ++nbLocalAxis[j];
              setTmp.insert(recvBuff[k]);
            }
          }
        }

        MPI_Bcast(&nbLocalAxis[0], nbAxis, MPI_INT,
                  0, context->intraComm_);
      }

      int nbAxisDistributedPart = 1;
      for (int j = 0; j < nbAxis; ++j) nbAxisDistributedPart *= nbLocalAxis[j];
      nbDomainDistributedPart_ = clientSize/nbAxisDistributedPart;
    }
  }
}
CATCH

/*!
  Compute the distribution of the domain destination by using available information provided by user such as n_distributed_partition of axis
*/
void CDomainAlgorithmGenerateRectilinear::computeDistributionGridDestination(CGrid* gridDest)
TRY
{
  // For now, just suppose that the grid contains only one domain
  std::vector<CAxis*> axisListDestP = gridDest->getAxis();
  int nbPartition = 1, idx = 0;
  for (int i = 0; i < gridDest->axis_domain_order.numElements(); ++i)
  {
    if (false == (gridDest->axis_domain_order)(i))
    {
      nbPartition *= (axisListDestP[idx]->n_distributed_partition.isEmpty()) ? 1: (axisListDestP[idx]->n_distributed_partition.getValue());
      ++idx;
    }
  }

  CContext* context = CContext::getCurrent();
  int modPart = (context->intraCommSize_) % nbPartition;
  if (0 != modPart)
    ERROR("CDomainAlgorithmGenerateRectilinear::computeDistributionGridDestination(CGrid* gridDest)",
       << "The grid " <<gridDest->getId() << " is not well-distributed. There is an incompatibility between distribution of axis and domain.");
  nbDomainDistributedPart_ = context->intraCommSize_/nbPartition;

}
CATCH

/*!
  Fill in all necessary attributes of domain destination and their values
*/
void CDomainAlgorithmGenerateRectilinear::fillInAttributesDomainDestination()
TRY
{
  if (!domainDest_->distributionAttributesHaveValue())
    domainDest_->redistribute(nbDomainDistributedPart_);
  domainDest_->fillInLonLat();
}
CATCH
}
