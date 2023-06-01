#include "domain_algorithm_extract.hpp"
#include "extract_domain.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "attribute_template.hpp"

namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CDomainAlgorithmExtract::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

  return make_shared<CDomainAlgorithmExtract>(isSource, domainListDestP[domainDstIndex], domainListSrcP[domainSrcIndex], extractDomain);
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
   // Reset geometrical attributes to avoid incompatible (user/domainSource) attributs
   //   attributs will be defined using domainSource and/or transformation attributs
   domainDestination->type.reset();
   domainDestination->ni_glo.reset();
   domainDestination->nj_glo.reset();
   
   domainDestination->i_index.reset();   // defined using domainSource->getLocalElement()
   domainDestination->j_index.reset();   // "
   domainDestination->ibegin.reset();    // will be computed in domainDestination->checkDomain() (from checkAttributes())
   domainDestination->ni.reset();        // "
   domainDestination->jbegin.reset();    // "
   domainDestination->nj.reset();        // "
   
   domainDestination->mask_1d.reset();   // defined scanning domainSource->getFullView() & domainSource->getWorkflowView() differencies
   domainDestination->mask_2d.reset();   //   in all case domainDestination->mask_1d used as reference  

   // domainDestination->data_* attributes will be computed in :
   domainDestination->data_dim.reset();     // domainDestination->checkDomainData() (from checkAttributes())
   domainDestination->data_ni.reset();
   domainDestination->data_nj.reset();
   domainDestination->data_ibegin.reset();
   domainDestination->data_jbegin.reset();
   domainDestination->data_i_index.reset(); // domainDestination->checkCompression() (from checkAttributes())
   domainDestination->data_j_index.reset();

   // Next attributes will be set using domainSource->attributes 
   domainDestination->lonvalue_1d.reset();
   domainDestination->latvalue_1d.reset();
   domainDestination->lonvalue_2d.reset();
   domainDestination->latvalue_2d.reset();
   domainDestination->nvertex.reset();
   domainDestination->bounds_lon_1d.reset();
   domainDestination->bounds_lat_1d.reset();
   domainDestination->bounds_lon_2d.reset();
   domainDestination->bounds_lat_2d.reset();
   domainDestination->area_1d.reset();
   domainDestination->area_2d.reset();
   domainDestination->radius.reset();
   
 
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
  int ind, indLocSrc, indLocDest, iIdxSrc, jIdxSrc, destIBegin = -1, destJBegin = -1, niDest = 0, njDest = 0 ;
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

   
  // Set attributes for this transformation
  domainDest_->type = domainSrc_ -> type ;
  domainDest_->ni_glo.setValue(extractNi_);
  domainDest_->nj_glo.setValue(extractNj_);
  domainDest_->i_index.resize(niDest*njDest);
  domainDest_->j_index.resize(niDest*njDest);

  if (!domainSrc_->nvertex.isEmpty()) domainDest_->nvertex = domainSrc_->nvertex ;

  // Resize lon/lat, bounds, area arrays to local domain dimensions
  if (!domainSrc_->lonvalue_1d.isEmpty())
  {
    if (domainDest_->type == CDomain::type_attr::rectilinear)
    {
      domainDest_->lonvalue_1d.resize(niDest);
      domainDest_->latvalue_1d.resize(njDest);
    }
    else 
    {
      domainDest_->lonvalue_1d.resize(niDest*njDest);
      domainDest_->latvalue_1d.resize(niDest*njDest);
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
      domainDest_->bounds_lat_2d.resize(domainDest_->nvertex, niDest, njDest);
    }
    else if (!domainSrc_->bounds_lon_1d.isEmpty())
    {
      domainDest_->bounds_lon_1d.resize(domainDest_->nvertex, niDest);
      domainDest_->bounds_lat_1d.resize(domainDest_->nvertex, niDest);
    }
  }
  if (domainSrc_->hasArea) 
  {
    if (!domainSrc_->area_2d.isEmpty()) domainDest_->area_2d.resize(niDest,njDest);
    else if (!domainSrc_->area_1d.isEmpty()) domainDest_->area_1d.resize(niDest*njDest);
  }
  // Set attributes required to define domainDestination->localElement_ and associated views, full and workflow)
  CArray<size_t,1> sourceGlobalIdx = domainSource->getLocalElement()->getGlobalIndex();
  int indexSize = sourceGlobalIdx.numElements();
  domainDest_->data_i_index.resize(niDest*njDest);
  domainDestination->data_i_index = -1; 
  domainDest_->data_j_index.resize(niDest*njDest);
  domainDestination->data_j_index = 0; 

  CArray<int,1> sourceWorkflowIdx = domainSource->getLocalView(CElementView::WORKFLOW)->getIndex();
  int srcWorkflowSize = sourceWorkflowIdx.numElements();

  int iIdxSrcMin = INT_MAX;
  int jIdxSrcMin = INT_MAX;
  int IdxMin = INT_MAX;
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    if ( sourceGlobalIdx(countSrc)%domainSource->ni_glo < iIdxSrcMin )
      iIdxSrcMin = sourceGlobalIdx(countSrc)%domainSource->ni_glo;
    if ( sourceGlobalIdx(countSrc)/domainSource->ni_glo < jIdxSrcMin )
      jIdxSrcMin = sourceGlobalIdx(countSrc)/domainSource->ni_glo;
    if ( sourceGlobalIdx(countSrc) < IdxMin )
      IdxMin = sourceGlobalIdx(countSrc);
  }
  int countDest(0); // increment of the position in destination domain 
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    int iIdxSrc = sourceGlobalIdx(countSrc)%domainSource->ni_glo;
    int jIdxSrc = sourceGlobalIdx(countSrc)/domainSource->ni_glo;
    // check that point countSrc concerned by extract
    if ( (iIdxSrc >= extractIBegin_) && (iIdxSrc <= extractIEnd_)
         && (jIdxSrc >= extractJBegin_) && (jIdxSrc <= extractJEnd_) )
    {
      // if concerned, convert source the global indexation in the extracted frame
      domainDest_->i_index(countDest) = iIdxSrc-extractIBegin_;
      domainDest_->j_index(countDest) = jIdxSrc-extractJBegin_;

      // ------------------ define transformation only if in the WF ------------------ 
      // countSrc+IdxMin is the global position (not index) considering the distributed memory
      //     - can be compared to the workflow view
      int iIdxSrc2 = (countSrc%domainSource->ni)+(IdxMin)%domainSource->ni_glo;
      int jIdxSrc2 = (countSrc/domainSource->ni)+(IdxMin)/domainSource->ni_glo;
      int convert_locally_global_idx = (jIdxSrc2-jIdxSrcMin)*domainSource->ni + (iIdxSrc2-iIdxSrcMin) ;
      bool concerned_by_WF(false);
      for ( int i = 0 ; i<sourceWorkflowIdx.numElements() ; ++i )
      {
        if (sourceWorkflowIdx(i)==convert_locally_global_idx)
        {      
          concerned_by_WF = true;
          break;
        }
      }
      if (concerned_by_WF)
      {
        transformationMapping_[extractNi_*(jIdxSrc-extractJBegin_)+iIdxSrc-extractIBegin_]=sourceGlobalIdx(countSrc);
        domainDest_->data_i_index( countDest ) = countDest;
      }
      // -----------------------------------------------------------------------------

      int iIdxDestLocal = countDest%niDest;
      int jIdxDestLocal = countDest/niDest;
      int iIdxSrcLocal  = countSrc%domainSource->ni;
      int jIdxSrcLocal  = countSrc/domainSource->ni;

      // area
      if (!domainSrc_->area_2d.isEmpty()) domainDest_->area_2d(iIdxDestLocal,jIdxDestLocal) = domainSrc_->area_2d(iIdxSrcLocal,jIdxSrcLocal);
      else if (!domainSrc_->area_1d.isEmpty())  domainDest_->area_1d(countDest) = domainSrc_->area_1d(countSrc);

      // bounds
      if (!domainDest_->bounds_lon_1d.isEmpty())
      {
        for (int n = 0; n < domainSrc_->nvertex; ++n)
        {
          domainDest_->bounds_lon_1d(n, countDest) = domainSrc_->bounds_lon_1d(n,countSrc);
          domainDest_->bounds_lat_1d(n, countDest) = domainSrc_->bounds_lat_1d(n,countSrc);
        }
      }
      else if (!domainDest_->bounds_lon_2d.isEmpty())
      {
        for (int n = 0; n < domainSrc_->nvertex; ++n)
        {
          domainDest_->bounds_lon_2d(n, iIdxDestLocal, jIdxDestLocal) = domainSrc_->bounds_lon_2d(n, iIdxSrcLocal, jIdxSrcLocal);
          domainDest_->bounds_lat_2d(n, iIdxDestLocal, jIdxDestLocal) = domainSrc_->bounds_lat_2d(n, iIdxSrcLocal, jIdxSrcLocal);
        }
      }

      // lon/lat
      if (!domainDest_->lonvalue_1d.isEmpty())
      {
        if (domainDest_->type == CDomain::type_attr::rectilinear)
        {
          // i : scan nbr of points in src
          domainDest_->lonvalue_1d(iIdxDestLocal) = domainSrc_->lonvalue_1d(iIdxSrcLocal);
          domainDest_->latvalue_1d(jIdxDestLocal) = domainSrc_->latvalue_1d(jIdxSrcLocal);
        }
        else
        {
          domainDest_->lonvalue_1d(countDest) = domainSrc_->lonvalue_1d(countSrc);
          domainDest_->latvalue_1d(countDest) = domainSrc_->latvalue_1d(countSrc);
        }
      }
      else if (!domainDest_->lonvalue_2d.isEmpty())
      {
        domainDest_->lonvalue_2d(iIdxDestLocal, jIdxDestLocal) = domainSrc_->lonvalue_2d(iIdxSrcLocal,jIdxSrcLocal);
        domainDest_->latvalue_2d(iIdxDestLocal, jIdxDestLocal) = domainSrc_->latvalue_2d(iIdxSrcLocal,jIdxSrcLocal);
      }
      
      // if point i has been identified as extracted, increment position in destination domain for the next point
      countDest++;
    }

  }
  
  domainDestination->checkAttributes() ;
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), domainDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH


}
