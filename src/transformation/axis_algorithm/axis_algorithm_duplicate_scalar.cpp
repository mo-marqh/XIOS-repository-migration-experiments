/*!
   \file axis_algorithm_duplicate_scalar.cpp

   \brief Algorithm to duplicate scalar into  axis 
 */
#include "axis_algorithm_duplicate_scalar.hpp"
#include "duplicate_scalar_to_axis.hpp"
#include "axis.hpp"
#include "scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"

namespace xios {
CGenericAlgorithmTransformation* CAxisAlgorithmDuplicateScalar::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
  std::vector<CScalar*> scalarListSrcP = gridSrc->getScalars();

  CDuplicateScalarToAxis* duplicateScalar = dynamic_cast<CDuplicateScalarToAxis*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int scalarSrcIndex = elementPositionInGridSrc2ScalarPosition[elementPositionInGrid];

  return (new CAxisAlgorithmDuplicateScalar(isSource, axisListDestP[axisDstIndex], scalarListSrcP[scalarSrcIndex], duplicateScalar));
}
CATCH

bool CAxisAlgorithmDuplicateScalar::dummyRegistered_ = CAxisAlgorithmDuplicateScalar::registerTrans();
bool CAxisAlgorithmDuplicateScalar::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_DUPLICATE_SCALAR_TO_AXIS, create);
}
CATCH


CAxisAlgorithmDuplicateScalar::CAxisAlgorithmDuplicateScalar(bool isSource, CAxis* axisDestination, CScalar* scalarSource, CDuplicateScalarToAxis* algo)
 : CAlgorithmTransformationTransfer(isSource)
{
  axisDestination->checkAttributes() ;
  
  CArray<int,1>& axisDstIndex = axisDestination->index;

  int nbAxisIdx = axisDstIndex.numElements();
  for (int idxAxis = 0; idxAxis < nbAxisIdx; ++idxAxis)
  {
    int globalAxisIdx = axisDstIndex(idxAxis);
    this->transformationMapping_[globalAxisIdx] = 0 ;
  }

  axisDestination->checkAttributes() ;
  this->computeAlgorithm(scalarSource->getLocalView(CElementView::WORKFLOW), axisDestination->getLocalView(CElementView::WORKFLOW)) ;
}


CAxisAlgorithmDuplicateScalar::~CAxisAlgorithmDuplicateScalar()
{
}

}
