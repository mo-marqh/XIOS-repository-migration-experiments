/*!
   \file domain_algorithm_zoom.cpp
   \author Ha NGUYEN
   \since 02 Jul 2015
   \date 02 Jul 2015

   \brief Algorithm for zooming on an domain.
 */
#include "domain_algorithm_zoom.hpp"

namespace xios {

CDomainAlgorithmZoom::CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource)
{
  zoomDomain->checkValid(domainSource);
  zoomIBegin_ = zoomDomain->zoom_ibegin.getValue();
  zoomJBegin_ = zoomDomain->zoom_jbegin.getValue();

  zoomNi_  = zoomDomain->zoom_ni.getValue();
  zoomNj_  = zoomDomain->zoom_nj.getValue();

  zoomIEnd_ = zoomIBegin_ + zoomNi_ - 1;
  zoomJEnd_ = zoomJBegin_ + zoomNj_ - 1;

  if (zoomNi_ > domainSource->ni_glo.getValue())
  {
    ERROR("CDomainAlgorithmZoom::CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain)",
           << "Zoom size is greater than size of domain source"
           << "Size ni_glo of domain source " <<domainSource->getId() << " is " << domainSource->ni_glo.getValue()  << std::endl
           << "Zoom size is " << zoomNi_ );
  }

  if (zoomNj_ > domainSource->nj_glo.getValue())
  {
    ERROR("CDomainAlgorithmZoom::CDomainAlgorithmZoom(CDomain* domainDestination, CDomain* domainSource, CZoomDomain* zoomDomain)",
           << "Zoom size is greater than size of domain source"
           << "Size nj_glo of domain source " <<domainSource->getId() << " is " << domainSource->nj_glo.getValue()  << std::endl
           << "Zoom size is " << zoomNj_ );
  }

  computeIndexSourceMapping();
}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmZoom::computeIndexSourceMapping()
{
  int niSource = domainSrc_->ni.getValue();
  int ibeginSource = domainSrc_->ibegin.getValue();
  int iendSource = ibeginSource + niSource - 1;

  int ibegin = std::max(ibeginSource, zoomIBegin_);
  int iend = std::min(iendSource, zoomIEnd_);
  int ni = iend + 1 - ibegin;
  if (iend < ibegin) ni = 0;

  int njSource = domainSrc_->nj.getValue();
  int jbeginSource = domainSrc_->jbegin.getValue();
  int jendSource = jbeginSource + njSource - 1;

  int jbegin = std::max(jbeginSource, zoomJBegin_);
  int jend = std::min(jendSource, zoomJEnd_);
  int nj = jend + 1 - jbegin;
  if (jend < jbegin) nj = 0;

  int niGlob = domainSrc_->ni_glo.getValue();
  int njGlob = domainSrc_->nj_glo.getValue();
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_;
  int domainGlobalIndex;
  for (int j = 0; j < nj; ++j)
  {
    for (int i = 0; i < ni; ++i)
    {
      domainGlobalIndex = (j+jbegin) * niGlob + (i+ibegin);
      transMap[domainGlobalIndex].push_back(domainGlobalIndex);
      transWeight[domainGlobalIndex].push_back(1.0);
    }
  }

  updateZoom();
  updateDomainDestinationMask();
}

/*!
  After a zoom on domain, it should be certain that (global) zoom begin and (global) zoom size are updated
*/
void CDomainAlgorithmZoom::updateZoom()
{
  domainDest_->global_zoom_ibegin = zoomIBegin_;
  domainDest_->global_zoom_jbegin = zoomJBegin_;
  domainDest_->global_zoom_ni  = zoomNi_;
  domainDest_->global_zoom_nj  = zoomNj_;
}

/*!
  Update mask on domain
  Because only zoomed region on domain is not masked, the remaining must be masked to make sure
correct index be extracted
*/
void CDomainAlgorithmZoom::updateDomainDestinationMask()
{
  int niMask     = domainDest_->ni.getValue();
  int iBeginMask = domainDest_->ibegin.getValue();
  int njMask     = domainDest_->nj.getValue();
  int jBeginMask = domainDest_->jbegin.getValue();
  int niGlob = domainDest_->ni_glo.getValue();
  int globalIndexMask = 0;

  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<int> >::const_iterator ite = (transMap).end();
  for (int j = 0; j < njMask; ++j)
  {
    for (int i = 0; i < niMask; ++i)
    {
      globalIndexMask = (j+jBeginMask) * niGlob + (i + iBeginMask);
      if (transMap.find(globalIndexMask) == ite)
        (domainDest_->mask_1d)(i+j*niMask) = false;
    }
  }
}

}
