/*!
   \file axis_algorithm_inverse.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 29 June 2015

   \brief Algorithm for inversing an axis..
 */
#include "axis_algorithm_inverse.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "axis.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "inverse_axis.hpp"
#include "client_client_dht_template.hpp"

namespace xios {

CGenericAlgorithmTransformation* CAxisAlgorithmInverse::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

  CInverseAxis* inverseAxis = dynamic_cast<CInverseAxis*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return (new CAxisAlgorithmInverse(isSource, axisListDestP[axisDstIndex], axisListSrcP[axisSrcIndex], inverseAxis));
}
CATCH

bool CAxisAlgorithmInverse::dummyRegistered_ = CAxisAlgorithmInverse::registerTrans();
bool CAxisAlgorithmInverse::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_INVERSE_AXIS, create);
}
CATCH

CAxisAlgorithmInverse::CAxisAlgorithmInverse(bool isSource, CAxis* axisDestination, CAxis* axisSource, CInverseAxis* inverseAxis)
 : CAlgorithmTransformationNoDataModification(isSource), axisDest_(axisDestination), axisSrc_(axisSource)
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
  axisDestination->bounds.reset();

  axisDestination->n_glo.setValue( axisSource->n_glo.getValue() );

  if (axisDestination->n_glo.getValue() != axisSource->n_glo.getValue())
  {
    ERROR("CAxisAlgorithmInverse::CAxisAlgorithmInverse(CAxis* axisDestination, CAxis* axisSource)",
           << "Two axis have different global size"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->n_glo.getValue()  << std::endl
           << "Size of axis destination " <<axisDestination->getId() << " is " << axisDestination->n_glo.getValue());
  }

  int n_glo =axisDestination->n_glo.getValue() ;
  
  CArray<size_t,1> sourceGlobalIdx = axisSource->getLocalElement()->getGlobalIndex();
  int indexSize = sourceGlobalIdx.numElements();
  
  axisDestination->index.resize( indexSize );
  axisDestination->data_index.resize( indexSize );
  axisDestination->data_index = -1;
  for (size_t i = 0; i < indexSize ; ++i)
  {
    axisDestination->index(i) = (n_glo-1)-sourceGlobalIdx(i);
  }

  int idxMin = INT_MAX;
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    if ( sourceGlobalIdx(countSrc) < idxMin )
      idxMin = sourceGlobalIdx(countSrc);
  }

  CArray<int,1> sourceWorkflowIdx = axisSource->getLocalView(CElementView::WORKFLOW)->getIndex();
  int srcWorkflowSize = sourceWorkflowIdx.numElements();
  for (size_t i = 0; i < indexSize ; ++i)
  {
    // ------------------ define transformation only if in the WF ------------------ 
    int iIdxSrc2 = (i+idxMin)%axisSource->n_glo;
    int convert_locally_global_idx = (iIdxSrc2-idxMin) ;
    bool concerned_by_WF(false);
    int i2;
    for ( i2 = 0 ; i2<sourceWorkflowIdx.numElements() ; ++i2 )
    {
      if (sourceWorkflowIdx(i2)==convert_locally_global_idx)
      {
	concerned_by_WF = true;
	break;
      }
    }

    if (concerned_by_WF)
      {
	axisDestination->data_index(sourceWorkflowIdx(i2)) = sourceWorkflowIdx(i2);
      }
    // -----------------------------------------------------------------------------

  }

  if (axisSrc_->hasValue)
  {
    axisDestination->value.resize(indexSize);
    for (size_t i = 0; i < indexSize ; ++i)
    {
      axisDestination->value(i) = axisSource->value(i);
    }
  }

  if (axisSrc_->hasBounds)
  {
    axisDestination->bounds.resize(2,indexSize);
    for (int i = 0; i < indexSize ; ++i)
    {
      axisDestination->bounds(0,i) = axisSource->bounds(0,i);
      axisDestination->bounds(1,i) = axisSource->bounds(1,i);
    }
  }

  axisDestination->checkAttributes() ;

}
CATCH

}
