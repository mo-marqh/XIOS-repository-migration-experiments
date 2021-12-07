/*!
   \file axis_algorithm_reduce_domain.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for extract a domain to an axis
 */
#include "axis_algorithm_extract_domain.hpp"
#include "extract_domain_to_axis.hpp"
#include "axis.hpp"
#include "domain.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "reduction.hpp"

namespace xios
{

shared_ptr<CGenericAlgorithmTransformation> CAxisAlgorithmExtractDomain::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
  std::vector<CDomain*> domainListSrcP = gridSrc->getDomains();

  CExtractDomainToAxis* extractDomain = dynamic_cast<CExtractDomainToAxis*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int domainSrcIndex = elementPositionInGridSrc2DomainPosition[elementPositionInGrid];

  return make_shared<CAxisAlgorithmExtractDomain>(isSource, axisListDestP[axisDstIndex], domainListSrcP[domainSrcIndex], extractDomain);
}
CATCH

bool CAxisAlgorithmExtractDomain::dummyRegistered_ = CAxisAlgorithmExtractDomain::registerTrans();
bool CAxisAlgorithmExtractDomain::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_EXTRACT_DOMAIN_TO_AXIS, create);
}
CATCH


CAxisAlgorithmExtractDomain::CAxisAlgorithmExtractDomain(bool isSource, CAxis* axisDestination, CDomain* domainSource, CExtractDomainToAxis* algo)
 : CAlgorithmTransformationTransfer(isSource), pos_(-1), axisDest_(axisDestination), domainSrc_(domainSource)
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

  algo->checkValid(axisDestination, domainSource);
  StdString op = "extract";

  int nglo,nloc;
  switch (algo->direction)
  {
    case CExtractDomainToAxis::direction_attr::jDir:
      dir_ = jDir;
      nglo = domainSource->nj_glo.getValue();
      nloc = domainSource->nj.getValue();
      break;
    case CExtractDomainToAxis::direction_attr::iDir:
      dir_ = iDir;
      nglo = domainSource->ni_glo.getValue();
      nloc = domainSource->ni.getValue();
      break;
    default:
      break;
  }
  
  axisDestination->n_glo.setValue( nglo );
  axisDestination->index.resize( nloc );
  axisDestination->data_index.resize( nloc );
  axisDestination->data_index = -1;

  if ( axisDestination->index.isEmpty() )
  {
    axisDestination->n.setValue( 0 );
    axisDestination->begin.setValue( 0 );
  }

  pos_ = algo->position;

  auto& transMap = this->transformationMapping_;
  
  CArray<size_t,1> sourceGlobalIdx = domainSource->getLocalElement()->getGlobalIndex();
  int indexSize = sourceGlobalIdx.numElements();

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
  
  if (jDir == dir_)
  {
    int countDest(0);
    for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
    {
      if ( sourceGlobalIdx(countSrc)%domainSource->ni_glo == pos_ )
      {
	axisDest_->index(countDest) = sourceGlobalIdx(countSrc)/domainSource->ni_glo;
	int iIdxSrc2 = (countSrc+IdxMin)%domainSource->ni_glo;
	int jIdxSrc2 = (countSrc+IdxMin)/domainSource->ni_glo;
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
	  axisDest_->data_index(countDest) = countDest;
	  transMap[axisDest_->index(countDest)] = sourceGlobalIdx(countSrc);
	}
        countDest++;
      }
    }
  }
  else if (iDir == dir_)
  {
    int countDest(0);
    for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
    {
      if ( sourceGlobalIdx(countSrc)/domainSource->ni_glo == pos_ )
      {
        axisDest_->index(countDest) = sourceGlobalIdx(countSrc)%domainSource->ni_glo;
	int iIdxSrc2 = (countSrc+IdxMin)%domainSource->ni_glo;
	int jIdxSrc2 = (countSrc+IdxMin)/domainSource->ni_glo;
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
	  axisDest_->data_index(countDest) = countDest;
	  transMap[axisDest_->index(countDest)] = sourceGlobalIdx(countSrc);
	}
        countDest++;
      }
    }
  }
  else
  {}

  axisDestination->checkAttributes() ;
  this->computeAlgorithm(domainSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH


CAxisAlgorithmExtractDomain::~CAxisAlgorithmExtractDomain()
TRY
{
}
CATCH

}
