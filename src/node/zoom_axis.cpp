#include "zoom_axis.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CZoomAxis::CZoomAxis(void)
    : CObjectTemplate<CZoomAxis>(), CZoomAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CZoomAxis::CZoomAxis(const StdString & id)
    : CObjectTemplate<CZoomAxis>(id), CZoomAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CZoomAxis::~CZoomAxis(void)
  {}

  //----------------------------------------------------------------

  StdString CZoomAxis::GetName(void)    { return StdString("zoom_axis"); }
  StdString CZoomAxis::GetDefName(void) { return StdString("zoom_axis"); }
  ENodeType CZoomAxis::GetType(void)    { return eZoomAxis; }

  void CZoomAxis::checkValid(CAxis* axisDest)
  {
    int axisIBegin, axisNi, axisGlobalSize;
    int zoom_begin, zoom_end, zoom_size;

    axisIBegin = axisDest->begin.getValue();
    axisNi     = axisDest->n.getValue();
    axisGlobalSize   = axisDest->n_glo.getValue();

    zoom_begin = (this->zoom_begin.isEmpty()) ?  0 : this->zoom_begin.getValue() ;
    zoom_size  = (this->zoom_size.isEmpty()) ?  axisGlobalSize : this->zoom_size.getValue() ;
    zoom_end   = (this->zoom_end.isEmpty()) ?  (axisGlobalSize - 1) : this->zoom_end.getValue() ;

    if (this->zoom_begin.isEmpty()) zoom_begin=zoom_end-zoom_size+1;
    if (this->zoom_size.isEmpty()) zoom_size=zoom_end-zoom_begin+1;
    if (this->zoom_end.isEmpty()) zoom_end=zoom_begin+zoom_size-1;

    if (zoom_begin < 0 || zoom_begin > axisGlobalSize - 1 || zoom_end < 0 || zoom_end > axisGlobalSize - 1
        || zoom_size < 1 || zoom_size > axisGlobalSize || zoom_begin > zoom_end)
      ERROR("CZoomAxis::checkValid(CAxis* axisDest)",
            << "One or more attributes among 'zoom_begin' (" << zoom_begin << "), 'zoom_end' (" << zoom_end << "), 'zoom_size' (" << zoom_size << ") "
            << "of axis transformation [ id = '" << axisDest->getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] are not well specified");

    this->zoom_begin.setValue(zoom_begin) ;
    this->zoom_end.setValue(zoom_end) ;
    this->zoom_size.setValue(zoom_size) ;

  }

}
