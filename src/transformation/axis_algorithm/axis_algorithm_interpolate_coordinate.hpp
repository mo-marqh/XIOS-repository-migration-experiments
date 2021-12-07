/*!
   \file axis_algorithm_interpolate.hpp
   \author Ha NGUYEN
   \since 23 June 2015
   \date 23 June 2015

   \brief Algorithm for interpolation on an axis.
 */
#ifndef __XIOS_AXIS_ALGORITHM_INTERPOLATE_COORDINATE_HPP__
#define __XIOS_AXIS_ALGORITHM_INTERPOLATE_COORDINATE_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios {

class CAxis;
class CGrid;
class CInterpolateAxis;
class CTransformFilter;

/*!
  \class CAxisAlgorithmInterpolateCoordinate
  Implementing interpolation on axis
  The values on axis source are assumed monotonic
*/
class CAxisAlgorithmInterpolateCoordinate : public CAlgorithmTransformationTransfer
{
public:
  CAxisAlgorithmInterpolateCoordinate(bool isSource, CAxis* axisDestination, CAxis* axisSource, CInterpolateAxis* interpAxis);

  virtual ~CAxisAlgorithmInterpolateCoordinate() {}
  virtual vector<string> getAuxFieldId(void)  ;
  virtual bool transformAuxField(int pos) ;
  virtual void apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, 
                     const vector<CArray<double,1>>& auxDataIn, CArray<double,1>& dataOut) ;
  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\ninterpolate_axis";}
  virtual CTransformFilter* createTransformFilter(CGarbageCollector& gc, shared_ptr<CGridAlgorithm> algo, bool detectMissingValues, double defaultValue) ;
  
private:
  void computeInterp(int nsrc, vector<double>& srcCoordinate, vector<double>& srcValue, vector<int>& srcIndex,
                     int ndst, vector<double>& dstCoordinate, vector<double>& dstValue, vector<int>& dstIndex) ;

  // Interpolation order
  int order_;
  StdString coordinateSrc_; // pressure src
  StdString coordinateDest_; // pressure dst
  bool hasCoordinate_=false ;
  bool hasCoordinateSrc_=false ;
  bool hasCoordinateDest_=false ;

  CAxis* axisSrc_=nullptr ;
  CAxis* axisDest_=nullptr;
  size_t ngloSrc_ ;
  size_t nDest_ ;
  vector<double> srcCoordinate_  ; // src axis value
  vector<double> destCoordinate_ ; // dst axis value

public:
  static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CAxis>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
  static bool dummyRegistered_;
};

}

#endif // __XIOS_AXIS_ALGORITHM_INTERPOLATE_HPP__
