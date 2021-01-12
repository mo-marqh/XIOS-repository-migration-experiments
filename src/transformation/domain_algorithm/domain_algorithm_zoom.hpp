
#ifndef __XIOS_DOMAIN_ALGORITHM_ZOOM_HPP__
#define __XIOS_DOMAIN_ALGORITHM_ZOOM_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios {

class CDomain;
class CZoomDomain;

/*!
  \class CDomainAlgorithmZoom
  Implementing zoom (alternative zoom) on domain
*/
class CDomainAlgorithmZoom : public CAlgorithmTransformationTransfer
{
public:
  CDomainAlgorithmZoom(bool isSource, CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain);

  virtual ~CDomainAlgorithmZoom() {}

  static bool registerTrans();
protected:
  void updateDomainAttributes();
 
private:
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
  CDomain* domainSrc_ ;
  CDomain* domainDest_ ;


public:
  static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CDomain>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
  static bool dummyRegistered_;
};

}
#endif // __XIOS_DOMAIN_ALGORITHM_ZOOM_HPP__
