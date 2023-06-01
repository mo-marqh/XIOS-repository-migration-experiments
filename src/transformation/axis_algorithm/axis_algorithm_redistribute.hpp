/*!
   \brief Algorithm for redistributing axis.
 */
#ifndef __XIOS_AXIS_ALGORITHM_REDISTRIBUTE_HPP__
#define __XIOS_AXIS_ALGORITHM_REDISTRIBUTE_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios 
{

  class CAxis;
  class CRedistributeAxis;

  /*!
    \class CAxisAlgorithmReorder
    Redistribute data on axis
  */
  class CAxisAlgorithmRedistribute : public CAlgorithmTransformationTransfer  
  {
    public:
      CAxisAlgorithmRedistribute(bool isSource, CAxis* axisDestination, CAxis* axisSource, CRedistributeAxis* redistributeAxis);

      virtual ~CAxisAlgorithmRedistribute() {}

      static bool registerTrans();
      virtual StdString getAlgoName() {return "\\nredistribute_axis";}

  
    public:
      static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                     CTransformation<CAxis>* transformation,
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
