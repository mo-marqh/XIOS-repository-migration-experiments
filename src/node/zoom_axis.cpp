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
    StdSize axisIBegin, axisNi, axisSize;
    StdSize zoom_begin,zoom_end, zoom_size;

    axisIBegin = axisDest->ibegin.getValue();
    axisNi     = axisDest->ni.getValue();
    axisSize   = axisDest->size.getValue();

    zoom_begin = (this->zoom_begin.isEmpty()) ?  0 : this->zoom_begin.getValue() ;
    zoom_size  = (this->zoom_size.isEmpty()) ?  axisSize : this->zoom_size.getValue() ;
    zoom_end   = (this->zoom_end.isEmpty()) ?  (axisSize - 1) : this->zoom_end.getValue() ;

    if (this->zoom_begin.isEmpty()) zoom_begin=zoom_end-zoom_size+1 ;
    if (this->zoom_end.isEmpty()) zoom_end=zoom_begin+zoom_size-1 ;
    if (this->zoom_size.isEmpty()) zoom_size=zoom_end-zoom_begin+1 ;

    if ((zoom_begin < 0) || (zoom_begin > axisSize-1) || (zoom_end<0) || (zoom_end>axisSize-1) || (zoom_size<1) || (zoom_size>axisSize) || (zoom_begin>zoom_end))
      ERROR("CZoomAxis::checkAttributes(void)",
            << "One or more attributes among <zoom_begin>, <zoom_end>, <zoom_size> of axis transformation [ id = '" << axisDest->getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] are not well specified");

    this->zoom_begin.setValue(zoom_begin) ;
    this->zoom_end.setValue(zoom_end) ;
    this->zoom_size.setValue(zoom_size) ;

  }

}
