#include "interpolate_axis.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CInterpolateAxis::CInterpolateAxis(void)
    : CObjectTemplate<CInterpolateAxis>(), CInterpolateAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CInterpolateAxis::CInterpolateAxis(const StdString & id)
    : CObjectTemplate<CInterpolateAxis>(id), CInterpolateAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CInterpolateAxis::~CInterpolateAxis(void)
  {}

  //----------------------------------------------------------------

  StdString CInterpolateAxis::GetName(void)    { return StdString("interpolate_axis"); }
  StdString CInterpolateAxis::GetDefName(void) { return StdString("interpolate_axis"); }
  ENodeType CInterpolateAxis::GetType(void)    { return eInterpolateAxis; }

  void CInterpolateAxis::checkValid(CAxis* axisDest)
  {
  }

}
