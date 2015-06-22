#ifndef __XIOS_AXIS_ALGORITHM_INVERSE_HPP__
#define __XIOS_AXIS_ALGORITHM_INVERSE_HPP__

#include "axis_algorithm_transformation.hpp"
#include "axis.hpp"

namespace xios {

class CAxisInverse : public CAxisAlgorithmTransformation //public CConcreteAlgo
{
public:
  CAxisInverse(CAxis* axisDestination, CAxis* axisSource);

  virtual ~CAxisInverse() {}

  virtual void computeIndexSourceMapping();
};

}
#endif // __XIOS_AXIS_ALGORITHM_INVERSE_HPP__
