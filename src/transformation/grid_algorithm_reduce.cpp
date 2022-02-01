#include "grid_algorithm_reduce.hpp"
#include "algo_types.hpp"

namespace xios
{
 
  void CGridAlgorithmReduce::apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut)
  {
    CArray<double,1> dataOutTmp ;
    gridTransformConnector_->transfer(dataIn, dataOutTmp, operator_) ;
    algorithm_->apply(dimBefore_, dimAfter_, dataOutTmp, dataOut) ;
  }
}