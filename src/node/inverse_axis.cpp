#include "inverse_axis.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CInverseAxis::CInverseAxis(void)
    : CObjectTemplate<CInverseAxis>(), CInverseAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CInverseAxis::CInverseAxis(const StdString & id)
    : CObjectTemplate<CInverseAxis>(id), CInverseAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CInverseAxis::~CInverseAxis(void)
  {}

  //----------------------------------------------------------------

  StdString CInverseAxis::GetName(void)    { return StdString("inverse_axis"); }
  StdString CInverseAxis::GetDefName(void) { return StdString("inverse_axis"); }
  ENodeType CInverseAxis::GetType(void)    { return eInverseAxis; }

  void CInverseAxis::checkValid(CAxis* axisSrc)
  {}

}
