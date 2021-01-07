#ifndef __XIOS_GRID_ALGORITHM_NO_DATA_MODIFICATION_HPP__
#define __XIOS_GRID_ALGORITHM_NO_DATA_MODIFICATION_HPP__

#include "grid_algorithm.hpp"
namespace xios
{
  class CGrid;
  class CGenericAlgorithmTransformation ;

  /*!
  \class CGenericAlgorithmTransformation
  This class defines the interface for all other inherited algorithms class
  */
class CTransformFilter ;
class CGarbageCollector ;
class CGenericAlgorithmTransformation ;

class CGridAlgorithmNoDataModification : public CGridAlgorithm
{
  public:
    CGridAlgorithmNoDataModification(CGenericAlgorithmTransformation* algo) : CGridAlgorithm(algo) {}
    ~CGridAlgorithmNoDataModification(void) {};
    void computeAlgorithm(void) {} ;
    virtual void apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut) ;
};

}
#endif // __XIOS_GRID_ALGORITHM_NO_DATA_MODIFICATION_HPP__
