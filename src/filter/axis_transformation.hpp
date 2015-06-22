#ifndef __XIOS_AXIS_TRANSFORMATION_HPP__
#define __XIOS_AXIS_TRANSFORMATION_HPP__

#include "visitable.hpp"
#include "axis.hpp"

namespace xios {

class CAxisTransformation : public CGenericTransformation
{
public:
  DEFINE_VISITABLE()
public:
  /** Default constructor */
  CAxisTransformation(std::vector<CAxis*> inputs, std::vector<CAxis*> outputs);

  std::vector<CAxis*> getOutputs();
  std::vector<CAxis*> getInputs();
protected:
  void setInputs(const std::vector<CAxis*>& inputs);
  void setOutputs(const std::vector<CAxis*>& outputs);

protected:
  std::vector<CAxis*> inputs_;
  std::vector<CAxis*> outputs_;

};

}
#endif // __XIOS_AXIS_TRANSFORMATION_HPP__
