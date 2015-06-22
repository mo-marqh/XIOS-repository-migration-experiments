#include "generic_filter.hpp"

namespace xios {

CGenericFilter::CGenericFilter(CGrid* gridInput_, CGrid* gridOutput_)
 : gridInput(gridInput_), gridOutput(gridOutput_), distributionClient_(0)
{
}

CGenericFilter::~CGenericFilter()
{
  if (0 != distributionClient_) delete distributionClient_;
}

void CGenericFilter::setInputs(const std::vector<CField*>& inputs)
{
  fieldInputs_ = inputs;
}

void CGenericFilter::setOutputs(const std::vector<CField*>& outputs)
{
  fieldOutputs_ = outputs;
}

void CGenericFilter::setOutput(CField* output)
{
  fieldOutputs_.resize(1);
  fieldOutputs_[0] = output;
}

const std::vector<CField*> CGenericFilter::getInputs()
{
  return fieldInputs_;
}

std::vector<CField*> CGenericFilter::getOutputs()
{
  return fieldOutputs_;
}

}
