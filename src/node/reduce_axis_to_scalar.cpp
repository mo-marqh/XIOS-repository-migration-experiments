#include "reduce_axis_to_scalar.hpp"
#include "scalar_algorithm_reduce_axis.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CReduceAxisToScalar::CReduceAxisToScalar(CContext* context)
    : CObjectTemplate<CReduceAxisToScalar>(context), CReduceAxisToScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CReduceAxisToScalar::CReduceAxisToScalar(CContext* context, const StdString & id)
    : CObjectTemplate<CReduceAxisToScalar>(context, id), CReduceAxisToScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CReduceAxisToScalar::~CReduceAxisToScalar(void)
  {}

  CTransformation<CScalar>* CReduceAxisToScalar::create(CContext* context, const StdString& id, xml::CXMLNode* node)
  {
    CReduceAxisToScalar* reduceAxis = CReduceAxisToScalarGroup::get(context, "reduce_axis_to_scalar_definition")->createChild(id);
    if (node) reduceAxis->parse(*node);
    return static_cast<CTransformation<CScalar>*>(reduceAxis);
  }

  bool CReduceAxisToScalar::registerTrans()
  {
    return registerTransformation(TRANS_REDUCE_AXIS_TO_SCALAR, {create, getTransformation});
  }

  bool CReduceAxisToScalar::_dummyRegistered = CReduceAxisToScalar::registerTrans();

  //----------------------------------------------------------------

  StdString CReduceAxisToScalar::GetName(void)    { return StdString("reduce_axis_to_scalar"); }
  StdString CReduceAxisToScalar::GetDefName(void) { return StdString("reduce_axis_to_scalar"); }
  ENodeType CReduceAxisToScalar::GetType(void)    { return eReduceAxisToScalar; }

  void CReduceAxisToScalar::checkValid(CScalar* scalarDst)
  {
  }

  shared_ptr<CGenericAlgorithmTransformation> CReduceAxisToScalar::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CScalarAlgorithmReduceAxis::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
