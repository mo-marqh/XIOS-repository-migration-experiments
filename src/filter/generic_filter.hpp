#ifndef __XIOS_GENERIC_FILTER_HPP__
#define __XIOS_GENERIC_FILTER_HPP__

#include "visitable.hpp"
#include "field.hpp"
#include "grid.hpp"
#include "distribution_client.hpp"

namespace xios {

class CGenericFilter : public virtual CBaseVisitable<>
{
public:
  DEFINE_VISITABLE()
public:
  CGenericFilter(CGrid* gridInput, CGrid* gridOutput);

  virtual ~CGenericFilter();

  void setInputs(const std::vector<CField*>& fieldInputs);
  void setOutputs(const std::vector<CField*>& fieldOutputs);
  void setOutput(CField* fieldOutput);

  const std::vector<CField*> getInputs();
  std::vector<CField*> getOutputs();

public:
  CGrid* gridInput;
  CGrid* gridOutput;

protected:
  std::vector<CField*> fieldInputs_;
  std::vector<CField*> fieldOutputs_;
  CDistributionClient* distributionClient_;
};

}
#endif // __XIOS_GENERIC_FILTER_HPP__
