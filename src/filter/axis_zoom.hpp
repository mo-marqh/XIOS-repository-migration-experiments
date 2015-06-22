#ifndef __XIOS_AXIS_ALGORITHM_ZOOM_HPP__
#define __XIOS_AXIS_ALGORITHM_ZOOM_HPP__

#include "concrete_algo.hpp"
#include "axis.hpp"

namespace xios {

class CAxisZoom : public CConcreteAlgo
{
public:
  CAxisZoom(CAxis* axisDestination, CAxis* axisSource);

  virtual ~CAxisZoom() {}

  virtual void computeIndexSourceMapping(const std::map<int, std::vector<int> >& transformationMappingOfPreviousAlgo);

protected:
  std::vector<int> axisDestGlobalIndex_;

private:
  int axisDestGlobalSize_;
};

}
#endif // __XIOS_AXIS_ALGORITHM_ZOOM_HPP__
