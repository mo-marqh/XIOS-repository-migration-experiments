#include "redistribute_axis.hpp"
#include "axis_algorithm_redistribute.hpp"
#include "type.hpp"
#include "field.hpp"

namespace xios
{

  /// ////////////////////// Définitions ////////////////////// ///

  CRedistributeAxis::CRedistributeAxis(CContext* context)
    : CObjectTemplate<CRedistributeAxis>(context), CRedistributeAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CRedistributeAxis::CRedistributeAxis(CContext* context, const StdString & id)
    : CObjectTemplate<CRedistributeAxis>(context, id), CRedistributeAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CRedistributeAxis::~CRedistributeAxis(void)
  {}

  CTransformation<CAxis>* CRedistributeAxis::create(CContext* context, const StdString& id, xml::CXMLNode* node)
  {
    CRedistributeAxis* redistributeAxis = CRedistributeAxisGroup::get(context, "redistribute_axis_definition")->createChild(id);
    if (node) redistributeAxis->parse(*node);
    return static_cast<CTransformation<CAxis>*>(redistributeAxis);
  }

  bool CRedistributeAxis::registerTrans()
  {
    return registerTransformation(TRANS_REDISTRIBUTE_AXIS, {create, getTransformation});
  }

  bool CRedistributeAxis::_dummyRegistered = CRedistributeAxis::registerTrans();

  //----------------------------------------------------------------

  StdString CRedistributeAxis::GetName(void)    { return StdString("redistribute_axis"); }
  StdString CRedistributeAxis::GetDefName(void) { return StdString("redistribute_axis"); }
  ENodeType CRedistributeAxis::GetType(void)    { return eRedistributeAxis; }

  void CRedistributeAxis::checkValid(CAxis* axisSrc)
  {

  }

  shared_ptr<CGenericAlgorithmTransformation> CRedistributeAxis::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
        
    return CAxisAlgorithmRedistribute::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid, 
                elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
