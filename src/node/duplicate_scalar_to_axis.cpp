#include "duplicate_scalar_to_axis.hpp"
#include "axis_algorithm_duplicate_scalar.hpp"
#include "type.hpp"
#include "axis.hpp"
#include "scalar.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CDuplicateScalarToAxis::CDuplicateScalarToAxis(void)
    : CObjectTemplate<CDuplicateScalarToAxis>(), CDuplicateScalarToAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CDuplicateScalarToAxis::CDuplicateScalarToAxis(const StdString & id)
    : CObjectTemplate<CDuplicateScalarToAxis>(id), CDuplicateScalarToAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CDuplicateScalarToAxis::~CDuplicateScalarToAxis(void)
  {}

  CTransformation<CAxis>* CDuplicateScalarToAxis::create(const StdString& id, xml::CXMLNode* node)
  {
    CDuplicateScalarToAxis* duplicateScalar = CDuplicateScalarToAxisGroup::get("duplicate_scalar_to_axis_definition")->createChild(id);
    if (node) duplicateScalar->parse(*node);
    return static_cast<CTransformation<CAxis>*>(duplicateScalar);
  }

  bool CDuplicateScalarToAxis::registerTrans()
  {
    return registerTransformation(TRANS_DUPLICATE_SCALAR_TO_AXIS, {create, getTransformation});
  }

  bool CDuplicateScalarToAxis::_dummyRegistered = CDuplicateScalarToAxis::registerTrans();

  //----------------------------------------------------------------

  StdString CDuplicateScalarToAxis::GetName(void)    { return StdString("duplicate_scalar_to_axis"); }
  StdString CDuplicateScalarToAxis::GetDefName(void) { return StdString("duplicate_scalar_to_axis"); }
  ENodeType CDuplicateScalarToAxis::GetType(void)    { return eDuplicateScalarToAxis; }

  void CDuplicateScalarToAxis::checkValid(CAxis* axisDst, CScalar* scalarSrc)
  {
   
  }
  
  shared_ptr<CGenericAlgorithmTransformation> CDuplicateScalarToAxis::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                         std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CAxisAlgorithmDuplicateScalar::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid, 
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition) ;
  }
}
