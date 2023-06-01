#include "redistribute_scalar.hpp"
#include "scalar_algorithm_redistribute.hpp"
#include "type.hpp"
#include "field.hpp"

namespace xios
{

  /// ////////////////////// Définitions ////////////////////// ///

  CRedistributeScalar::CRedistributeScalar(void)
    : CObjectTemplate<CRedistributeScalar>(), CRedistributeScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CRedistributeScalar::CRedistributeScalar(const StdString & id)
    : CObjectTemplate<CRedistributeScalar>(id), CRedistributeScalarAttributes(), CTransformation<CScalar>()
  { /* Ne rien faire de plus */ }

  CRedistributeScalar::~CRedistributeScalar(void)
  {}

  CTransformation<CScalar>* CRedistributeScalar::create(const StdString& id, xml::CXMLNode* node)
  {
    CRedistributeScalar* redistributeScalar = CRedistributeScalarGroup::get("redistribute_scalar_definition")->createChild(id);
    if (node) redistributeScalar->parse(*node);
    return static_cast<CTransformation<CScalar>*>(redistributeScalar);
  }

  bool CRedistributeScalar::registerTrans()
  {
    return registerTransformation(TRANS_REDISTRIBUTE_SCALAR, {create, getTransformation});
  }

  bool CRedistributeScalar::_dummyRegistered = CRedistributeScalar::registerTrans();

  //----------------------------------------------------------------

  StdString CRedistributeScalar::GetName(void)    { return StdString("redistribute_scalar"); }
  StdString CRedistributeScalar::GetDefName(void) { return StdString("redistribute_scalar"); }
  ENodeType CRedistributeScalar::GetType(void)    { return eRedistributeScalar; }

  void CRedistributeScalar::checkValid(CScalar* scalarSrc)
  {

  }

  shared_ptr<CGenericAlgorithmTransformation> CRedistributeScalar::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
       
    return CScalarAlgorithmRedistribute::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid, 
                elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
