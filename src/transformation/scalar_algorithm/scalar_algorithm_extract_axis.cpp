/*!
   \file scalar_algorithm_extract_scalar.cpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for extract an axis to a scalar
 */
#include "scalar_algorithm_extract_axis.hpp"
#include "axis.hpp"
#include "scalar.hpp"
#include "extract_axis_to_scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

#include "reduction.hpp"

namespace xios {
shared_ptr<CGenericAlgorithmTransformation> CScalarAlgorithmExtractAxis::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                                     CTransformation<CScalar>* transformation,
                                                                     int elementPositionInGrid,
                                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition)
TRY
{
  std::vector<CScalar*> scalarListDestP = gridDst->getScalars();
  std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

  CExtractAxisToScalar* extractAxis = dynamic_cast<CExtractAxisToScalar*> (transformation);
  int scalarDstIndex = elementPositionInGridDst2ScalarPosition[elementPositionInGrid];
  int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

  return make_shared<CScalarAlgorithmExtractAxis>(isSource, scalarListDestP[scalarDstIndex], axisListSrcP[axisSrcIndex], extractAxis);
}
CATCH

bool CScalarAlgorithmExtractAxis::dummyRegistered_ = CScalarAlgorithmExtractAxis::registerTrans();
bool CScalarAlgorithmExtractAxis::registerTrans()
TRY
{
  return CGridTransformationFactory<CScalar>::registerTransformation(TRANS_EXTRACT_AXIS_TO_SCALAR, create);
}
CATCH

CScalarAlgorithmExtractAxis::CScalarAlgorithmExtractAxis(bool isSource, CScalar* scalarDestination, CAxis* axisSource, CExtractAxisToScalar* algo)
 : CAlgorithmTransformationTransfer(algo->getContext(), isSource)
TRY
{
  scalarDestination->mask.reset();
  scalarDestination->value.reset();
  scalarDestination->n.reset();

  algo->checkValid(scalarDestination, axisSource);
  pos_ = algo->position;

  scalarDestination->mask.setValue( false ); // scalar do not contain data_index, WF view set looking at mask
  scalarDestination->n.setValue( 0 );

  // ----------------------------
  bool scalarOnThisAxisPiece(false);
  CArray<size_t,1> sourceGlobalIdx = axisSource->getLocalElement()->getGlobalIndex();
  int indexSize = sourceGlobalIdx.numElements();
  int idxSrc(-1);
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    if ( sourceGlobalIdx(countSrc) == pos_ )
    {
      scalarOnThisAxisPiece = true;
      idxSrc = countSrc;
    }
  }
   
  int idxMin = INT_MAX;
  for (int countSrc = 0; countSrc < indexSize ; ++countSrc)
  {
    if ( sourceGlobalIdx(countSrc) < idxMin )
      idxMin = sourceGlobalIdx(countSrc);
  }
  
  CArray<int,1> sourceWorkflowIdx = axisSource->getLocalView(CElementView::WORKFLOW)->getIndex();
  int srcWorkflowSize = sourceWorkflowIdx.numElements();
  

  if (scalarOnThisAxisPiece)
  {
    int iIdxSrc2 = (idxSrc+idxMin)%axisSource->n_glo;
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

    // Check if in WF
    if (concerned_by_WF)
    {
      this->transformationMapping_[0]=pos_ ;
      scalarDestination->mask.setValue( true );
      scalarDestination->n.setValue( 1 );
    }

    if (axisSource->hasValue())
    {
      scalarDestination->value = axisSource->value(idxSrc);
    }
  }
  // ----------------------------
  
  scalarDestination->checkAttributes() ;
  this->computeAlgorithm(axisSource->getLocalView(CElementView::WORKFLOW), scalarDestination->getLocalView(CElementView::WORKFLOW)) ;
}
CATCH



}
