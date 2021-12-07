/*!
   \file axis_algorithm_extract.cpp
   \brief Algorithm for extracting an axis.
 */
#include "axis_algorithm_extract.hpp"
#include "axis.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "extract_axis.hpp"

namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CAxisAlgorithmExtract::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                           CTransformation<CAxis>* transformation,
                                                           int elementPositionInGrid,
                                                           std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                           std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                           std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                           std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                           std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                           std::map<int, int>& elementPositionInGridDst2DomainPosition)
TRY
{
  std::vector<CAxis*> axisListDestP = gridDst->getAxis();
  std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

  CExtractAxis* extractAxis = dynamic_cast<CExtractAxis*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return make_shared<CAxisAlgorithmExtract>(isSource, axisListDestP[axisDstIndex], axisListSrcP[axisSrcIndex], extractAxis);
}
CATCH

bool CAxisAlgorithmExtract::dummyRegistered_ = CAxisAlgorithmExtract::registerTrans();
bool CAxisAlgorithmExtract::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_EXTRACT_AXIS, create);
}
CATCH

CAxisAlgorithmExtract::CAxisAlgorithmExtract(bool isSource, CAxis* axisDestination, CAxis* axisSource, CExtractAxis* extractAxis)
: CAlgorithmTransformationTransfer(isSource), axisDest_(axisDestination), axisSrc_(axisSource)
TRY
{
  axisDestination->axis_type.reset();
  axisDestination->n_glo.reset();
  axisDestination->index.reset();
  axisDestination->n.reset();
  axisDestination->begin.reset();

  axisDestination->mask.reset();
  axisDestination->data_index.reset();
  axisDestination->data_n.reset();
  axisDestination->data_begin.reset();

  axisDestination->value.reset();
  axisDestination->label.reset();
  axisDestination->bounds.reset();

  extractAxis->checkValid(axisSource);
  extractBegin_ = extractAxis->begin.getValue();
  extractN_  = extractAxis->n.getValue();
  extractEnd_   = extractBegin_ + extractN_ - 1;

  if (extractN_ > axisSource->n_glo.getValue())
  {
    ERROR("CAxisAlgorithmExtract::CAxisAlgorithmExtract(CAxis* axisDestination, CAxis* axisSource, CExtractAxis* extractAxis)",
           << "Extract size is greater than global size of source axis"
           << "Global size of source axis " <<axisSource->getId() << " is " << axisSource->n_glo.getValue()  << std::endl
           << "Extract size is " << extractN_ );
  }

  int idxSrc, nDest = 0, beginDestLoc, beginDestGlo = 0 ;
  int indGloDest, indGloSrc, iSrc;
  for (int i = 0; i < axisSrc_->n.getValue(); i++)
  {
    idxSrc = axisSrc_->index(i);
    if ((idxSrc >= extractBegin_) && (idxSrc <= extractEnd_))
    {
      if (nDest == 0) beginDestLoc = i;
      ++nDest;
    }
  }
  
  axisDest_->n_glo.setValue(extractN_);
  axisDest_->index.resize(nDest);
  if (nDest==0)
  {
    axisDest_->n.setValue( 0 );
    axisDest_->begin.setValue( 0 );
  }

  if (axisSrc_->hasValue) axisDest_->value.resize(nDest);
  if (axisSrc_->hasLabel) axisDest_->label.resize(nDest);
  if (axisSrc_->hasBounds) axisDest_->bounds.resize(2,nDest);

  auto& transMap = this->transformationMapping_;

  // Set attributes required to define domainDestination->localElement_ and associated views, full and workflow)
  CArray<size_t,1> sourceGlobalIdx = axisSource->getLocalElement()->getGlobalIndex();
  int indexSize = sourceGlobalIdx.numElements();

  CArray<int,1> sourceWorkflowIdx = axisSource->getLocalView(CElementView::WORKFLOW)->getIndex();
  int srcWorkflowSize = sourceWorkflowIdx.numElements();
  axisDest_->data_index.resize(nDest);
  axisDest_->data_index = -1;

  int idxMin = INT_MAX;
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    if ( sourceGlobalIdx(countSrc) < idxMin )
      idxMin = sourceGlobalIdx(countSrc);
  }

  int countDest(0); // increment of the position in destination domain 
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    int idxSrc = sourceGlobalIdx(countSrc);
    if ( (idxSrc >= extractBegin_) && (idxSrc <= extractEnd_) )
    {
      axisDest_->index(countDest) = idxSrc-extractBegin_;

      // ------------------ define transformation only if in the WF ------------------ 
      int iIdxSrc2 = (countSrc+idxMin)%axisSource->n_glo;
      int convert_locally_global_idx = (iIdxSrc2-idxMin) ;
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
        transformationMapping_[idxSrc-extractBegin_]=sourceGlobalIdx(countSrc);
        axisDest_->data_index( countDest ) = countDest;
      }
      // -----------------------------------------------------------------------------

      if (axisSrc_->hasValue)
      {
        axisDest_->value(countDest) = axisSrc_->value(countSrc);
      }
      if (axisSrc_->hasLabel)
      {
        axisDest_->label(countDest) = axisSrc_->label(countSrc);
      }
      if (axisSrc_->hasBounds)
      {
        axisDest_->bounds(0,countDest) = axisSrc_->bounds(0,countSrc);
        axisDest_->bounds(1,countDest) = axisSrc_->bounds(1,countSrc);
      }
      
      // if point i has been identified as extracted, increment position in destination domain for the next point
      countDest++;
    }
  }
  
  axisDestination->checkAttributes() ;

  this->computeAlgorithm(axisSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH



}
