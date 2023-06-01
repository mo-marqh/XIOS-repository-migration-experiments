/*!
   \brief Algorithm for redistributing domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_REDISTRIBUTE_HPP__
#define __XIOS_DOMAIN_ALGORITHM_REDISTRIBUTE_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios 
{

  class CDomain;
  class CRedistributeDomain;

  /*!
    \class CDomainAlgorithmReorder
    Redistribute data on domain
  */
  class CDomainAlgorithmRedistribute : public CAlgorithmTransformationTransfer  
  {
    public:
      CDomainAlgorithmRedistribute(bool isSource, CDomain* domainDestination, CDomain* domainSource, CRedistributeDomain* redistributeDomain);

      virtual ~CDomainAlgorithmRedistribute() {}

      static bool registerTrans();
      virtual StdString getAlgoName() {return "\\nredistribute_domain";}

  
    public:
      static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                     CTransformation<CDomain>* transformation,
                                                     int elementPositionInGrid,
                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition);
      static bool dummyRegistered_;
};

}
#endif // __XIOS_DOMAIN_ALGORITHM_REDISTRIBUTE_HPP__
