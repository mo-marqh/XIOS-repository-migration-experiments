#ifndef __XIOS_AXIS_ALGORITHM_ZOOM_HPP__
#define __XIOS_AXIS_ALGORITHM_ZOOM_HPP__

#include "axis_algorithm_transformation.hpp"
#include "axis.hpp"
#include "zoom_axis.hpp"

namespace xios {

class CAxisAlgorithmZoom : public CAxisAlgorithmTransformation
{
public:
  CAxisAlgorithmZoom(CAxis* axisDestination, CAxis* axisSource, CZoomAxis* zoomAxis);

  virtual ~CAxisAlgorithmZoom() {}

  virtual void computeIndexSourceMapping();

private:
  void updateAxisDestinationMask();
  void updateZoom();
private:
  StdSize zoomBegin_;
  StdSize zoomEnd_;
  StdSize zoomSize_;

private:
  CAxis* axisDest_;
  CAxis* axisSrc_;
};

}
#endif // __XIOS_AXIS_ALGORITHM_ZOOM_HPP__
