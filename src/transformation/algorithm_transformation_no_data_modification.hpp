#ifndef __XIOS_ALGORITHM_TRANSFORMATION_NO_DATA_MODIFICATION_HPP__
#define __XIOS_ALGORITHM_TRANSFORMATION_NO_DATA_MODIFICATION_HPP__

#include "generic_algorithm_transformation.hpp"
#include "array_new.hpp"

namespace xios
{

  class CAlgorithmTransformationNoDataModification : public CGenericAlgorithmTransformation
  {
    public:

      CAlgorithmTransformationNoDataModification(bool isSource) : CGenericAlgorithmTransformation(isSource) {}
      virtual ~CAlgorithmTransformationNoDataModification() {};
      virtual void apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut);
      virtual shared_ptr<CGridAlgorithm> createGridAlgorithm(CGrid* gridSrc, CGrid* newGrid, int pos) ;
  };

}
#endif //__XIOS_ALGORITHM_TRANSFORMATION_NO_DATA_MODIFICATION_HPP__
