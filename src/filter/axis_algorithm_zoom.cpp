#include "axis_algorithm_zoom.hpp"

namespace xios {

CAxisAlgorithmZoom::CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)
: CAxisAlgorithmTransformation(axisDestination, axisSource), axisDest_(axisDestination), axisSrc_(axisSource)
{
  zoomAxis->checkValid(axisSource);
  zoomBegin_ = zoomAxis->zoom_begin.getValue();
  zoomEnd_   = zoomAxis->zoom_end.getValue();
  zoomSize_  = zoomAxis->zoom_size.getValue();

  if (zoomSize_ > axisSource->size.getValue())
  {
    ERROR("CAxisAlgorithmZoom::CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)",
           << "Zoom size is greater than size of axis source"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->size.getValue()  << std::endl
           << "Zoom size is " << zoomSize_ );
  }

  computeIndexSourceMapping();
}

void CAxisAlgorithmZoom::computeIndexSourceMapping()
{
  StdSize niSource = axisSrc_->ni.getValue();
  StdSize ibeginSource = axisSrc_->ibegin.getValue();
  StdSize iendSource = ibeginSource + niSource - 1;

  StdSize ibegin = std::max(ibeginSource, zoomBegin_);
  StdSize iend = std::min(iendSource, zoomEnd_);
  StdSize ni = iend + 1 - ibegin;
  if (iend < ibegin) ni = 0;

  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  for (StdSize idx = 0; idx < ni; ++idx)
  {
    transMap[ibegin+idx].push_back(ibegin+idx);
  }

  updateZoom();
  updateAxisDestinationMask();
}

void CAxisAlgorithmZoom::updateZoom()
{
  axisDest_->global_zoom_begin = zoomBegin_;
  axisDest_->global_zoom_size  = zoomSize_;
}

void CAxisAlgorithmZoom::updateAxisDestinationMask()
{
  StdSize niMask = axisDest_->mask.numElements();
  StdSize iBeginMask = axisDest_->ibegin.getValue();
  StdSize globalIndexMask = 0;
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<int> >::const_iterator ite = (transMap).end();
  for (StdSize idx = 0; idx < niMask; ++idx)
  {
    globalIndexMask = iBeginMask + idx;
    if (transMap.find(globalIndexMask) == ite)
      (axisDest_->mask)(idx) = false;
  }
}

}
