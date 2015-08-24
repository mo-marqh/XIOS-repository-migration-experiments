/*!
   \file axis_algorithm_zoom.cpp
   \author Ha NGUYEN
   \since 03 June 2015
   \date 12 June 2015

   \brief Algorithm for zooming on an axis.
 */
#include "axis_algorithm_zoom.hpp"

namespace xios {

CAxisAlgorithmZoom::CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)
: CAxisAlgorithmTransformation(axisDestination, axisSource)
{
  zoomAxis->checkValid(axisSource);
  zoomBegin_ = zoomAxis->zoom_begin.getValue();
  zoomEnd_   = zoomAxis->zoom_end.getValue();
  zoomSize_  = zoomAxis->zoom_size.getValue();

  if (zoomSize_ > axisSource->n_glo.getValue())
  {
    ERROR("CAxisAlgorithmZoom::CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis)",
           << "Zoom size is greater than global size of axis source"
           << "Global size of axis source " <<axisSource->getId() << " is " << axisSource->n_glo.getValue()  << std::endl
           << "Zoom size is " << zoomSize_ );
  }

  computeIndexSourceMapping();
}

/*!
  Compute the index mapping between axis on grid source and one on grid destination
*/
void CAxisAlgorithmZoom::computeIndexSourceMapping()
{
  StdSize niSource = axisSrc_->n.getValue();
  StdSize ibeginSource = axisSrc_->begin.getValue();
  StdSize iendSource = ibeginSource + niSource - 1;

  StdSize ibegin = std::max(ibeginSource, zoomBegin_);
  StdSize iend = std::min(iendSource, zoomEnd_);
  StdSize ni = iend + 1 - ibegin;
  if (iend < ibegin) ni = 0;

  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_;
  for (StdSize idx = 0; idx < ni; ++idx)
  {
    transMap[ibegin+idx].push_back(ibegin+idx);
    transWeight[ibegin+idx].push_back(1.0);
  }

  updateZoom();
  updateAxisDestinationMask();
}

/*!
  After a zoom on axis, it should be certain that (global) zoom begin and (global) zoom size are updated
*/
void CAxisAlgorithmZoom::updateZoom()
{
  axisDest_->global_zoom_begin = zoomBegin_;
  axisDest_->global_zoom_size  = zoomSize_;
}

/*!
  Update mask on axis
  Because only zoomed region on axis is not masked, the remaining must be masked to make sure
correct index be extracted
*/
void CAxisAlgorithmZoom::updateAxisDestinationMask()
{
  StdSize niMask = axisDest_->mask.numElements();
  StdSize iBeginMask = axisDest_->begin.getValue();
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
