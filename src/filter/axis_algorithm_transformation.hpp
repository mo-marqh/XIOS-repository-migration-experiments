#ifndef __XIOS_AXIS_ALGORITHM_TRANSFORMATION_HPP__
#define __XIOS_AXIS_ALGORITHM_TRANSFORMATION_HPP__

#include "generic_algorithm_transformation.hpp"
#include "axis.hpp"
#include "concrete_algo.hpp"

namespace xios {

class CAxisAlgorithmTransformation : public virtual CGenericAlgorithmTransformation
{
public:
  CAxisAlgorithmTransformation(CAxis* axisDestination, CAxis* axisSource, std::vector<ETranformationType>&);

  virtual ~CAxisAlgorithmTransformation();

protected:
  virtual void computeGlobalIndexFromGlobalIndexElement(int axisDestGlobalIndex,
                                                        const std::vector<int>& axisSrcGlobalIndex,
                                                        int axisPositionInGrid,
                                                        const std::vector<int>& gridDestGlobalDim,
                                                        const CArray<size_t,1>& globalIndexGridDestSendToServer,
                                                        CArray<size_t,1>& globalIndexDestGrid,
                                                        std::vector<CArray<size_t,1> >& globalIndexSrcGrid);
  void computeIndexSourceMapping();
protected:
  std::vector<CConcreteAlgo*> algosOfAnAxis_;

};

}
#endif // __XIOS_AXIS_ALGORITHM_TRANSFORMATION_HPP__
