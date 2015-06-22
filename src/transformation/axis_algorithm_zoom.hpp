/*!
   \file axis_algorithm_zoom.hpp
   \author Ha NGUYEN
   \since 03 June 2015
   \date 12 June 2015

   \brief Algorithm for zooming on an axis.
 */
#ifndef __XIOS_AXIS_ALGORITHM_ZOOM_HPP__
#define __XIOS_AXIS_ALGORITHM_ZOOM_HPP__

#include "axis_algorithm_transformation.hpp"
#include "axis.hpp"
#include "zoom_axis.hpp"

namespace xios {
/*!
  \class CAxisAlgorithmZoom
  Implementing zoom on axis
  A zoomed region can be considered as region that isnt masked.
  Only this zoomed region is extracted to write on Netcdf.
*/
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
  //! Global zoom begin on axis
  StdSize zoomBegin_;

  //! Global zoom end on axis
  StdSize zoomEnd_;

  //! Global zoom size on axis
  StdSize zoomSize_;

private:
  //! Axis on grid destination
  CAxis* axisDest_;

  //! Axis on grid source
  CAxis* axisSrc_;
};

}
#endif // __XIOS_AXIS_ALGORITHM_ZOOM_HPP__
