#ifndef __XIOS_CONCRETE_ALGORITHM_HPP__
#define __XIOS_CONCRETE_ALGORITHM_HPP__

#include <map>
#include <vector>

namespace xios {

class CConcreteAlgo
{
public:
  CConcreteAlgo() : transformationMapping_() {}

  virtual ~CConcreteAlgo() {}

  virtual void computeIndexSourceMapping(const std::map<int, std::vector<int> >&) = 0;

  const std::map<int, std::vector<int> >& getTransformationMapping() const { return transformationMapping_; }

protected:
  std::map<int, std::vector<int> > transformationMapping_;
};

}
#endif // __XIOS_CONCRETE_ALGORITHM_HPP__
