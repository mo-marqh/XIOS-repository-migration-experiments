#include "reduce_axis_to_axis.hpp"
#include "axis_algorithm_reduce_axis.hpp"
#include "type.hpp"
#include "axis.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CReduceAxisToAxis::CReduceAxisToAxis(CContext* context)
    : CObjectTemplate<CReduceAxisToAxis>(context), CReduceAxisToAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CReduceAxisToAxis::CReduceAxisToAxis(CContext* context, const StdString & id)
    : CObjectTemplate<CReduceAxisToAxis>(context, id), CReduceAxisToAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CReduceAxisToAxis::~CReduceAxisToAxis(void)
  {}

  CTransformation<CAxis>* CReduceAxisToAxis::create(CContext* context, const StdString& id, xml::CXMLNode* node)
  {
    CReduceAxisToAxis* reduceAxis = CReduceAxisToAxisGroup::get(context, "reduce_axis_to_axis_definition")->createChild(id);
    if (node) reduceAxis->parse(*node);
    return static_cast<CTransformation<CAxis>*>(reduceAxis);
  }

  bool CReduceAxisToAxis::registerTrans()
  {
    return registerTransformation(TRANS_REDUCE_AXIS_TO_AXIS, {create, getTransformation});
  }

  bool CReduceAxisToAxis::_dummyRegistered = CReduceAxisToAxis::registerTrans();

  //----------------------------------------------------------------

  StdString CReduceAxisToAxis::GetName(void)    { return StdString("reduce_axis_to_axis"); }
  StdString CReduceAxisToAxis::GetDefName(void) { return StdString("reduce_axis_to_axis"); }
  ENodeType CReduceAxisToAxis::GetType(void)    { return eReduceAxisToAxis; }

  void CReduceAxisToAxis::checkValid(CAxis* axisDst, CAxis* axisSrc)
  {
  
    if (this->operation.isEmpty())
      ERROR("CReduceAxisToAxis::checkValid(CAxis* axisDst, CAxis* axisSrc)",
             << "An operation must be defined."
             << "Axis source " <<axisSrc->getId() << std::endl
             << "Axis destination " << axisDst->getId());


    if (axisDst->n_glo != axisSrc->n_glo)
       ERROR("CReduceAxisToAxis::checkValid(CAxis* axisDst, CAxis* axisSrc)",
            << "both axis should have same n_glo"
            << "Axis source " <<axisSrc->getId() << " has n_glo " << axisSrc->n_glo << std::endl
            << "Axis destination " << axisDst->getId() << " has n_glo " << axisDst->n_glo);
    
  }

  shared_ptr<CGenericAlgorithmTransformation> CReduceAxisToAxis::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CAxisAlgorithmReduceAxis::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
