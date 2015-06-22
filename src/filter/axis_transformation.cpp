#include "axis_transformation.hpp"

namespace xios {

CAxisTransformation::CAxisTransformation(std::vector<CAxis*> inputs, std::vector<CAxis*> outputs)
{
  this->setInputs(inputs);
  this->setOutputs(outputs);
}

void CAxisTransformation::setInputs(const std::vector<CAxis*>& inputs)
{
  inputs_ = inputs;
}

void CAxisTransformation::setOutputs(const std::vector<CAxis*>& outputs)
{
  outputs_ = outputs;
}

std::vector<CAxis*> CAxisTransformation::getInputs()
{
  return inputs_;
}

std::vector<CAxis*> CAxisTransformation::getOutputs()
{
  return outputs_;
}

}
