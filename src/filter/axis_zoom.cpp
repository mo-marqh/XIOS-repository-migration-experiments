#include "axis_zoom.hpp"

namespace xios {

CAxisZoom::CAxisZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)
: CAxisAlgorithmTransformation(axisDestination, axisSource), axisDest_(axisDestination), axisSrc_(axisSource)
{
  zoomAxis->checkValid(axisSource);
  zoomBegin_ = zoomAxis->zoom_begin.getValue();
  zoomEnd_   = zoomAxis->zoom_end.getValue();
  zoomSize_  = zoomAxis->zoom_size.getValue();

  if (zoomSize_ > axisSource->size.getValue())
  {
    ERROR("CAxisZoom::CAxisZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)",
           << "Zoom size is greater than size of axis source"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->size.getValue()  << std::endl
           << "Zoom size is " << zoomSize_ );
  }

  // Axis destination now must have new size equal to zoom size
  axisDestination->size.setValue(zoomSize_);
  computeIndexSourceMapping();
}

void CAxisZoom::computeIndexSourceMapping()
{
  StdSize niSource = axisSrc_->ni.getValue();
  StdSize ibeginSource = axisSrc_->ibegin.getValue();
  StdSize iendSource = ibeginSource + niSource - 1;

  StdSize ibegin = std::max(ibeginSource, zoomBegin_);
  StdSize iend = std::min(iendSource, zoomEnd_);
  StdSize ni = iend + 1 - ibegin;
  if (ibeginSource > zoomEnd_)
  {
    axisDest_->ibegin.setValue(0);
    axisDest_->ni.setValue(0);
  }
  else
  {
    axisDest_->ibegin.setValue(ibegin - zoomBegin_);
    axisDest_->ni.setValue(ni);
  }

  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  StdSize axisDestIBegin = axisDest_->ibegin.getValue();
  for (StdSize idx = 0; idx < ni; ++idx)
  {
    transMap[axisDestIBegin+idx].push_back(ibegin+idx);
  }
}

}
