#include "algorithm_transformation_no_data_modification.hpp"

namespace xios
{
  void CAlgorithmTransformationNoDataModification::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    dataOut.reference(dataIn) ;
  }
  
}