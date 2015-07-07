/*!
   \file domain_algorithm_zoom.hpp
   \author Ha NGUYEN
   \since 03 June 2015
   \date 12 June 2015

   \brief Algorithm for zooming on an domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_ZOOM_HPP__
#define __XIOS_DOMAIN_ALGORITHM_ZOOM_HPP__

#include "domain_algorithm_transformation.hpp"
#include "zoom_domain.hpp"
#include "domain.hpp"

namespace xios {
/*!
  \class CDomainAlgorithmZoom
  Implementing zoom on domain
  A zoomed region can be considered as region that isnt masked.
  Only this zoomed region is extracted to write on Netcdf.
*/
class CDomainAlgorithmZoom : public CDomainAlgorithmTransformation
{
public:
  CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain);

  virtual ~CDomainAlgorithmZoom() {}

  virtual void computeIndexSourceMapping();

private:
  void updateDomainDestinationMask();
  void updateZoom();

private:
  //! Global zoom begin on domain
  int zoomIBegin_;
  int zoomJBegin_;

  //! Global zoom end on domain
  int zoomIEnd_;
  int zoomJEnd_;

  //! Global zoom size on domain
  int zoomNi_;
  int zoomNj_;
};

}
#endif // __XIOS_DOMAIN_ALGORITHM_ZOOM_HPP__
