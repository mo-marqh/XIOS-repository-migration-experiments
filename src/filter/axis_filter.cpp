#include "axis_filter.hpp"
#include "context.hpp"
#include "context_client.hpp"

namespace xios {

CAxisFilter::CAxisFilter(CGrid* gridInput, CGrid* gridOutput)
 : CGenericFilter(gridInput, gridOutput), isGridTransformed_(false)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;
  this->distributionClient_ = new CDistributionClient(client->clientRank, gridInput);
}

void CAxisFilter::setGridTransformed()
{
  isGridTransformed_ = true;
}

bool CAxisFilter::isGridTransformed()
{
  return isGridTransformed_;
}

const CArray<size_t,1>& CAxisFilter::getGlobalDataIndexInput() const
{
  return this->distributionClient_->getGlobalDataIndexSendToServer();
}

}
