/*!
   \file axis_algorithm_temporal_splitting.cpp

   \brief Algorithm to split scalar into  axis by temporal accumulation
 */
#include "axis_algorithm_temporal_splitting.hpp"
#include "temporal_splitting.hpp"
#include "axis.hpp"
#include "scalar.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "temporal_transform_filter.hpp"

namespace xios {
CGenericAlgorithmTransformation* CAxisAlgorithmTemporalSplitting::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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

  CTemporalSplitting* temporalSplitting = dynamic_cast<CTemporalSplitting*> (transformation);
  int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
  int scalarSrcIndex = elementPositionInGridSrc2ScalarPosition[elementPositionInGrid];

  return (new CAxisAlgorithmTemporalSplitting(isSource, axisListDestP[axisDstIndex], scalarListSrcP[scalarSrcIndex], temporalSplitting));
}
CATCH

bool CAxisAlgorithmTemporalSplitting::dummyRegistered_ = CAxisAlgorithmTemporalSplitting::registerTrans();
bool CAxisAlgorithmTemporalSplitting::registerTrans()
TRY
{
  return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_TEMPORAL_SPLITTING, create);
}
CATCH

CAxisAlgorithmTemporalSplitting::CAxisAlgorithmTemporalSplitting(bool isSource, CAxis* axisDestination, CScalar* scalarSource, CTemporalSplitting* algo)
 : CAlgorithmTransformationNoDataModification(isSource)
{
  nrecords_ = axisDestination->n_glo ; // also axis must not be distributed, make more test later
}

CTransformFilter* CAxisAlgorithmTemporalSplitting::createTransformFilter(CGarbageCollector& gc, CGridAlgorithm* algo, bool detectMissingValues, double defaultValue)
{
  return new CTemporalTransformFilter(gc, 1, algo, nrecords_, detectMissingValues, defaultValue) ;
}

CAxisAlgorithmTemporalSplitting::~CAxisAlgorithmTemporalSplitting()
{
}


}
