#ifndef __XIOS_GRID_ALGORITHM_TRANSFORMATION_HPP__
#define __XIOS_GRID_ALGORITHM_TRANSFORMATION_HPP__

#include "grid_transform_connector.hpp"
#include <map>
#include "array_new.hpp"
#include "local_view.hpp"

namespace xios
{
  class CGrid;
  class CGenericAlgorithmTransformation ;

  /*!
  \class CGenericAlgorithmTransformation
  This class defines the interface for all other inherited algorithms class
  */
class CGridAlgorithm
{
  public:
    CGridAlgorithm(CGrid* gridSrc, CGrid* gridDst, int pos,  CGenericAlgorithmTransformation* algo) ;
    virtual void computeAlgorithm(void) ;
    
    void apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut) ;

  private:
    CGenericAlgorithmTransformation* algorithm_=nullptr ;
    CGridTransformConnector* gridTransformConnector_=nullptr ;
    CGrid* gridSrc_ = nullptr ;
    CGrid* gridDst_ = nullptr ;
    int pos_ ;
    int dimBefore_=1 ;
    int dimAfter_=1 ;

};

}
#endif // __XIOS_GRID_ALGORITHM_TRANSFORMATION_HPP__
