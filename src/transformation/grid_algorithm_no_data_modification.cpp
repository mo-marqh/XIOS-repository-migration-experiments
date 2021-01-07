#include "grid_algorithm_no_data_modification.hpp"

namespace xios
{
  void CGridAlgorithmNoDataModification::apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    dataOut.reference(dataIn) ;
  }
  
}