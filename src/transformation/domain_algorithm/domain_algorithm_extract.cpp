#include "domain_algorithm_extract.hpp"
#include "extract_domain.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "attribute_template.hpp"

namespace xios {
CGenericAlgorithmTransformation* CDomainAlgorithmExtract::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                             CTransformation<CDomain>* transformation,
                                                             int elementPositionInGrid,
                                                             std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                             std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                             std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                             std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                             std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                             std::map<int, int>& elementPositionInGridDst2DomainPosition)
TRY
{
  std::vector<CDomain*> domainListDestP = gridDst->getDomains();
  std::vector<CDomain*> domainListSrcP  = gridSrc->getDomains();

  CExtractDomain* extractDomain = dynamic_cast<CExtractDomain*> (transformation);
  int domainDstIndex = elementPositionInGridDst2DomainPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return (new CDomainAlgorithmExtract(isSource, domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], extractDomain));
}
CATCH

bool CDomainAlgorithmExtract::dummyRegistered_ = CDomainAlgorithmExtract::registerTrans();
bool CDomainAlgorithmExtract::registerTrans()
TRY
{
  return CGridTransformationFactory<CDomain>::registerTransformation(TRANS_EXTRACT_DOMAIN, create);
}
CATCH

CDomainAlgorithmExtract::CDomainAlgorithmExtract(bool isSource, CDomain* domainDestination, CDomain* domainSource, CExtractDomain* extractDomain)
: CAlgorithmTransformationTransfer(isSource), domainSrc_(domainSource), domainDest_(domainDestination)
TRY
{
  extractDomain->checkValid(domainSource);
  extractIBegin_ = extractDomain->ibegin.getValue();
  extractJBegin_ = extractDomain->jbegin.getValue();

  extractNi_  = extractDomain->ni.getValue();
  extractNj_  = extractDomain->nj.getValue();

  extractIEnd_ = extractIBegin_ + extractNi_ - 1;
  extractJEnd_ = extractJBegin_ + extractNj_ - 1;

  if (extractNi_ > domainSource->ni_glo.getValue())
  {
    ERROR("CDomainAlgorithmExtract::CDomainAlgorithmExtract(CDomain* domainDestination, CDomain* domainSource, CExtractDomain* extractDomain)",
           << "Extract size is greater than size of domain source"
           << "Size ni_glo of domain source " <<domainSource->getId() << " is " << domainSource->ni_glo.getValue()  << std::endl
           << "Extract size is " << extractNi_ );
  }

  if (extractNj_ > domainSource->nj_glo.getValue())
  {
    ERROR("CDomainAlgorithmExtract::CDomainAlgorithmExtract(CDomain* domainDestination, CDomain* domainSource, CExtractDomain* extractDomain)",
           << "Extract size is greater than size of domain source"
           << "Size nj_glo of domain source " <<domainSource->getId() << " is " << domainSource->nj_glo.getValue()  << std::endl
           << "Extract size is " << extractNj_ );
  }

  // Calculate the size of local domain
  int ind, indLocSrc, indLocDest, iIdxSrc, jIdxSrc, destIBegin = -1, destJBegin = -1, niDest = 0, njDest = 0, ibeginDest, jbeginDest ;
  int indGloDest, indGloSrc, niGloSrc = domainSrc_->ni_glo, iSrc, jSrc;
  for (int j = 0; j < domainSrc_->nj.getValue(); j++)
  {
    for (int i = 0; i < domainSrc_->ni.getValue(); i++)
    {
      ind = j*domainSrc_->ni + i;
      iIdxSrc = domainSrc_->i_index(ind);
      if ((iIdxSrc >= extractIBegin_) && (iIdxSrc <= extractIEnd_))
      {
        jIdxSrc = domainSrc_->j_index(ind);
        if ((jIdxSrc >= extractJBegin_) && (jIdxSrc <= extractJEnd_))
        {
          if ((niDest == 0) && (njDest == 0))
          {
            destIBegin = i;
            destJBegin = j;
          }
          if (i == destIBegin) ++njDest;
        }
        if (j == destJBegin) ++niDest;

      }
    }
  }
  ibeginDest = destIBegin + domainSrc_->ibegin - extractIBegin_;
  jbeginDest = destJBegin + domainSrc_->jbegin - extractJBegin_;
  if (niDest==0) ibeginDest=0 ;
  if (njDest==0) jbeginDest=0 ;
  
  domainDest_->type = domainSrc_ -> type ;
  domainDest_->data_dim = domainSrc_->data_dim ;
  domainDest_->ni_glo.setValue(extractNi_);
  domainDest_->nj_glo.setValue(extractNj_);
  domainDest_->ni.setValue(niDest);
  domainDest_->nj.setValue(njDest);
  domainDest_->ibegin.setValue(ibeginDest);
  domainDest_->jbegin.setValue(jbeginDest);
  domainDest_->i_index.resize(niDest*njDest);
  domainDest_->j_index.resize(niDest*njDest);

  domainDest_->data_ni.setValue(niDest);
  domainDest_->data_nj.setValue(njDest);
  domainDest_->data_ibegin.setValue(0);  // local position
  domainDest_->data_jbegin.setValue(0);  // local position
  domainDest_->data_i_index.resize(niDest*njDest); // local position
  domainDest_->data_j_index.resize(niDest*njDest); // local position

  //domainDest_->domainMask.resize(niDest*njDest);

  if (!domainSrc_->mask_2d.isEmpty()) domainDest_->mask_2d.resize(niDest,njDest);
  if (!domainSrc_->mask_1d.isEmpty()) domainDest_->mask_1d.resize(niDest*njDest);

  if (!domainSrc_->lonvalue_1d.isEmpty())
  {
    if (domainDest_->type == CDomain::type_attr::rectilinear)
    {
      domainDest_->lonvalue_1d.resize(niDest);
      domainDest_->latvalue_1d.resize(njDest);
    }
    else if (domainDest_->type == CDomain::type_attr::unstructured)
    {
      domainDest_->lonvalue_1d.resize(niDest);
      domainDest_->latvalue_1d.resize(niDest);
    }
  }
  else if (!domainSrc_->lonvalue_2d.isEmpty())
  {
    domainDest_->lonvalue_2d.resize(niDest,njDest);
    domainDest_->latvalue_2d.resize(niDest,njDest);
  }

  if (domainSrc_->hasBounds)
  {
    if (!domainSrc_->bounds_lon_2d.isEmpty())
    {
      domainDest_->bounds_lon_2d.resize(domainDest_->nvertex, niDest, njDest);
      domainDest_->bounds_lon_2d.resize(domainDest_->nvertex, niDest, njDest);
    }
    else if (!domainSrc_->bounds_lon_1d.isEmpty())
    {
      domainDest_->bounds_lon_1d.resize(domainDest_->nvertex, niDest);
      domainDest_->bounds_lon_1d.resize(domainDest_->nvertex, niDest);
    }
  }
  if (domainSrc_->hasArea) domainDest_->area.resize(niDest,njDest);
  
  for (int iDest = 0; iDest < niDest; iDest++)
  {
    iSrc = iDest + destIBegin;
    for (int jDest = 0; jDest < njDest; jDest++)
    {
      jSrc = jDest + destJBegin;
      ind = jSrc * domainSrc_->ni + iSrc;
      iIdxSrc = domainSrc_->i_index(ind);
      jIdxSrc = domainSrc_->j_index(ind);
      indLocDest = jDest*niDest + iDest;
      indGloDest = (jDest + jbeginDest)*extractNi_ + (iDest + ibeginDest);
      indLocSrc = (jDest+destJBegin)*domainSrc_->ni + (iDest+destIBegin);
      indGloSrc = (jIdxSrc )* niGloSrc + iIdxSrc;
      domainDest_->i_index(indLocDest) = iDest + ibeginDest;                                             // i_index contains global positions
      domainDest_->j_index(indLocDest) = jDest + jbeginDest;                                             // i_index contains global positions
      domainDest_->data_i_index(indLocDest) = (domainSrc_->data_dim == 1) ? indLocDest : iDest;          // data_i_index contains local positions
      domainDest_->data_j_index(indLocDest) = (domainSrc_->data_dim == 1) ? 0 :jDest;                    // data_i_index contains local positions
      //domainDest_->domainMask(indLocDest) = domainSrc_->domainMask(indLocSrc);

      if (!domainSrc_->mask_2d.isEmpty())
        domainDest_->mask_2d(iDest,jDest) = domainSrc_->mask_2d(iSrc,jSrc);

      if (!domainSrc_->mask_1d.isEmpty())
        domainDest_->mask_1d(indLocDest) = domainSrc_->mask_1d(indLocSrc);

      if (domainSrc_->hasArea)
        domainDest_->area(iDest,jDest) = domainSrc_->area(iSrc,jSrc);


      if (domainSrc_->hasBounds)
      {
        if (!domainSrc_->bounds_lon_2d.isEmpty())
        {
          for (int n = 0; n < domainSrc_->nvertex; ++n)
          {
            domainDest_->bounds_lon_2d(n,iDest,jDest) = domainSrc_->bounds_lon_2d(n,iSrc,jSrc);
            domainDest_->bounds_lat_2d(n,iDest,jDest) = domainSrc_->bounds_lat_2d(n,iSrc,jSrc);
          }
        }
        else if (!domainSrc_->bounds_lon_1d.isEmpty())
        {
          for (int n = 0; n < domainSrc_->nvertex; ++n)
          {
            domainDest_->bounds_lon_1d(n,iDest) = domainSrc_->bounds_lon_1d(n,iSrc);
            domainDest_->bounds_lat_1d(n,iDest) = domainSrc_->bounds_lat_1d(n,iSrc);
          }
        }
      }

      if (domainSrc_->hasLonLat)
      {
        if (domainDest_->type == CDomain::type_attr::rectilinear)
        {
          domainDest_->latvalue_1d(jDest) = domainSrc_->latvalue_1d(jSrc);
        }
        else if (domainDest_->type == CDomain::type_attr::curvilinear)
        {
          domainDest_->lonvalue_2d(iDest,jDest) = domainSrc_->lonvalue_2d(iSrc,jSrc);
          domainDest_->latvalue_2d(iDest,jDest) = domainSrc_->latvalue_2d(iSrc,jSrc);
        }
      }

      transformationMapping_[indGloDest]=indGloSrc;

    }
    if (domainSrc_->hasLonLat)
    {
      if (domainDest_->type == CDomain::type_attr::unstructured)
      {
        domainDest_->lonvalue_1d(iDest) = domainSrc_->lonvalue_1d(iSrc);
        domainDest_->latvalue_1d(iDest) = domainSrc_->latvalue_1d(iSrc);
      }
      else if (domainDest_->type == CDomain::type_attr::rectilinear)
      {
        domainDest_->lonvalue_1d(iDest) = domainSrc_->lonvalue_1d(iSrc);
      }
    }
  }
  
  domainDestination->checkAttributes() ;
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), domainDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH


}
