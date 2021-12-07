#include "algorithm_transformation_no_data_modification.hpp"
#include "grid_algorithm_no_data_modification.hpp"
#include "grid.hpp"

namespace xios
{
  void CAlgorithmTransformationNoDataModification::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    dataOut.reference(dataIn) ;
  }
  
  shared_ptr<CGridAlgorithm> CAlgorithmTransformationNoDataModification::createGridAlgorithm(CGrid* gridSrc, CGrid* gridDst, int pos)
  {
    return make_shared<CGridAlgorithmNoDataModification>(static_pointer_cast<CAlgorithmTransformationNoDataModification>(shared_from_this())) ;
  }
}