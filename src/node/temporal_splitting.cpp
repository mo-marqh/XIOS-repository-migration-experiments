#include "temporal_splitting.hpp"
#include "axis_algorithm_temporal_splitting.hpp"
#include "type.hpp"
#include "axis.hpp"
#include "domain.hpp"
#include "scalar.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CTemporalSplitting::CTemporalSplitting(void)
    : CObjectTemplate<CTemporalSplitting>(), CTemporalSplittingAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CTemporalSplitting::CTemporalSplitting(const StdString & id)
    : CObjectTemplate<CTemporalSplitting>(id), CTemporalSplittingAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CTemporalSplitting::~CTemporalSplitting(void)
  {}

  CTransformation<CAxis>* CTemporalSplitting::create(const StdString& id, xml::CXMLNode* node)
  {
    CTemporalSplitting* temporalSplitting = CTemporalSplittingGroup::get("temporal_splitting_definition")->createChild(id);
    if (node) temporalSplitting->parse(*node);
    return static_cast<CTransformation<CAxis>*>(temporalSplitting);
  }

  bool CTemporalSplitting::registerTrans()
  {
    return registerTransformation(TRANS_TEMPORAL_SPLITTING, {create, getTransformation});
  }

  bool CTemporalSplitting::_dummyRegistered = CTemporalSplitting::registerTrans();

  //----------------------------------------------------------------

  StdString CTemporalSplitting::GetName(void)    { return StdString("temporal_splitting"); }
  StdString CTemporalSplitting::GetDefName(void) { return StdString("temporal_splitting"); }
  ENodeType CTemporalSplitting::GetType(void)    { return eTemporalSplitting; }

  void CTemporalSplitting::checkValid(CAxis* axisDst, CScalar* scalarSrc)
  {

  }

  shared_ptr<CGenericAlgorithmTransformation> CTemporalSplitting::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    return CAxisAlgorithmTemporalSplitting::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid,
                       elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                       elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
