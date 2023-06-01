/*!
   \brief Algorithm for redistributing scalar.
 */
#ifndef __XIOS_SCALAR_ALGORITHM_REDISTRIBUTE_HPP__
#define __XIOS_SCALAR_ALGORITHM_REDISTRIBUTE_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios 
{

  class CScalar;
  class CRedistributeScalar;

  /*!
    \class CDomainAlgorithmReorder
    Redistribute data on domain
  */
  class CScalarAlgorithmRedistribute : public CAlgorithmTransformationTransfer  
  {
    public:
      CScalarAlgorithmRedistribute(bool isSource, CScalar* scalarDestination, CScalar* scalarSource, CRedistributeScalar* redistributeScalar);

      virtual ~CScalarAlgorithmRedistribute() {}

      static bool registerTrans();
      virtual StdString getAlgoName() {return "\\nredistribute_scalar";}

  
    public:
      static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                     CTransformation<CScalar>* transformation,
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
#endif // __XIOS_SCALAR_ALGORITHM_REDISTRIBUTE_HPP__
