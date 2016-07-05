#include "reduce_axis_to_scalar.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CReduceAxisToScalar::CReduceAxisToScalar(void)
    : CObjectTemplate<CReduceAxisToScalar>(), CReduceAxisToScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CReduceAxisToScalar::CReduceAxisToScalar(const StdString & id)
    : CObjectTemplate<CReduceAxisToScalar>(id), CReduceAxisToScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CReduceAxisToScalar::~CReduceAxisToScalar(void)
  {}

  CTransformation<CScalar>* CReduceAxisToScalar::create(const StdString& id, xml::CXMLNode* node)
  {
    CReduceAxisToScalar* reduceAxis = CReduceAxisToScalarGroup::get("reduce_axis_to_scalar_definition")->createChild(id);
    if (node) reduceAxis->parse(*node);
    return static_cast<CTransformation<CScalar>*>(reduceAxis);
  }

  bool CReduceAxisToScalar::registerTrans()
  {
    return registerTransformation(TRANS_REDUCE_AXIS_TO_SCALAR, CReduceAxisToScalar::create);
  }

  bool CReduceAxisToScalar::_dummyRegistered = CReduceAxisToScalar::registerTrans();

  //----------------------------------------------------------------

  StdString CReduceAxisToScalar::GetName(void)    { return StdString("reduce_axis_to_scalar"); }
  StdString CReduceAxisToScalar::GetDefName(void) { return StdString("reduce_axis_to_scalar"); }
  ENodeType CReduceAxisToScalar::GetType(void)    { return eReduceAxisToScalar; }

  void CReduceAxisToScalar::checkValid(CScalar* scalarDst)
  {
//    int axisIBegin, axisNi, axisGlobalSize;
//    int begin, end, n;
//
//    axisIBegin = axisDest->begin.getValue();
//    axisNi     = axisDest->n.getValue();
//    axisGlobalSize   = axisDest->n_glo.getValue();
//
//    begin = (this->begin.isEmpty()) ?  0 : this->begin.getValue();
//    n     = (this->n.isEmpty()) ?  axisGlobalSize : this->n.getValue();
//    end   = begin+n-1;
//
//    if (begin < 0 || begin > axisGlobalSize - 1 || end < 0 || end > axisGlobalSize - 1
//        || n < 1 || n > axisGlobalSize || begin > end)
//      ERROR("CReduceAxisToScalar::checkValid(CAxis* axisDest)",
//            << "One or more attributes among 'begin' (" << begin << "), 'end' (" << end << "), 'n' (" << n << ") "
//            << "of axis transformation [ id = '" << axisDest->getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] are not well specified");
//
//    this->begin.setValue(begin);
//    this->n.setValue(n);

  }

}
