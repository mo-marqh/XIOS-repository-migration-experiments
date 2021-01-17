#ifndef __XIOS_GRID_ALGORITHM_HPP__
#define __XIOS_GRID_ALGORITHM_HPP__

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
class CTransformFilter ;
class CGarbageCollector ;

class CGridAlgorithm
{
  public:
    CGridAlgorithm(CGenericAlgorithmTransformation* algorithm) : algorithm_(algorithm)  {} ;
    virtual ~CGridAlgorithm() {} ;
    virtual void apply(const CArray<double,1>& dataIn, CArray<double,1>& dataOut) { abort(); } //=0
    virtual void apply(const CArray<double,1>& dataIn, const vector<CArray<double,1>>& auxData, CArray<double,1>& dataOut) { abort(); } //=0
    virtual CTransformFilter* createTransformFilter(CGarbageCollector& gc, bool detectMissingValues, double defaultValue) ;
    virtual bool isGenerateTranformation(void) ;
   
    protected:
      CGenericAlgorithmTransformation* algorithm_=nullptr ;
};

}
#endif // __XIOS_GRID_ALGORITHM_TRANSFORMATION_HPP__
