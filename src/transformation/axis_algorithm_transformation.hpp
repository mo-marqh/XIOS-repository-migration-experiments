/*!
   \file axis_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 29 June 2015

   \brief Interface for all axis transformation algorithms.
 */
#ifndef __XIOS_AXIS_ALGORITHM_TRANSFORMATION_HPP__
#define __XIOS_AXIS_ALGORITHM_TRANSFORMATION_HPP__

#include "generic_algorithm_transformation.hpp"
#include "axis.hpp"

namespace xios {

/*!
  \class CAxisAlgorithmTransformation
  Algorithms for axis.
*/
class CAxisAlgorithmTransformation : public virtual CGenericAlgorithmTransformation
{
public:
  CAxisAlgorithmTransformation(CAxis* axisDestination, CAxis* axisSource);

  virtual ~CAxisAlgorithmTransformation();

protected:
  virtual void computeGlobalGridIndexFromGlobalIndexElement(int axisDestGlobalIndex,
                                                        const std::vector<int>& axisSrcGlobalIndex,
                                                        const std::vector<int>& destGlobalIndexPositionInGrid,
                                                        int axisPositionInGrid,
                                                        const std::vector<int>& gridDestGlobalDim,
                                                        const std::vector<int>& gridSrcGlobalDim,
                                                        const GlobalLocalMap& globalLocalIndexDestSendToServerMap,
                                                        std::vector<std::pair<size_t,int> >& globalLocalIndexDestMap,
                                                        std::vector<std::vector<size_t> >& globalIndexSrcGrid);

  void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs);

protected:
  //! Global index of an axis on grid destination
  std::vector<int> axisDestGlobalIndex_;

  //! Size of
  int axisDestGlobalSize_;

    //! Axis on grid destination
  CAxis* axisDest_;

  //! Axis on grid source
  CAxis* axisSrc_;
};

}
#endif // __XIOS_AXIS_ALGORITHM_TRANSFORMATION_HPP__
