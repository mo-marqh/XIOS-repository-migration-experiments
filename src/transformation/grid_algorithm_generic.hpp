#ifndef __XIOS_GRID_ALGORITHM_GENERIC_HPP__
#define __XIOS_GRID_ALGORITHM_GENERIC_HPP__

#include "grid_transform_connector.hpp"
#include <map>
#include "array_new.hpp"
#include "local_view.hpp"
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

class CGridAlgorithmGeneric : public CGridAlgorithm
{
  public:
    CGridAlgorithmGeneric(CGrid* gridSrc, CGrid* gridDst, int pos,  CGenericAlgorithmTransformation* algo) ;
    virtual ~CGridAlgorithmGeneric() {} ;

    void computeAlgorithm(void) ;
    virtual void apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut) ;
    virtual void apply(const CArray<double,1>& dataIn, const vector<CArray<double,1>>& auxData, CArray<double,1>& dataOut) ;
    
  protected:
    shared_ptr<CGridTransformConnector> gridTransformConnector_=nullptr ;
    CGrid* gridSrc_ = nullptr ;
    CGrid* gridDst_ = nullptr ;
    int pos_ ;
    int dimBefore_=1 ;
    int dimAfter_=1 ;

};

}
#endif // __XIOS_GRID_ALGORITHM_GENERIC_HPP__
