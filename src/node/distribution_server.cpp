#include "distribution_server.hpp"

namespace xios {
CDistributionServer::CDistributionServer(int rank, int dims, int nServer, CArray<size_t,1>* globalIndex)
  : CDistribution(rank, dims, globalIndex), nServer_(nServer), nGlobal_()
{

}

CDistributionServer::CDistributionServer(int rank, int nServer, const std::vector<int>& nGlobal)
  : CDistribution(rank, nGlobal.size()), nServer_(nServer)
{
  readDistributionInfo(nGlobal);
  createGlobalIndex();
}

CDistributionServer::~CDistributionServer()
{
  if (0 != this->globalIndex_) delete globalIndex_;
}

void CDistributionServer::readDistributionInfo(const std::vector<int>& nGlobal)
{
  if (nGlobal.empty())
  {
    //! TODO: This error must be replaced a call to function processing scalar value
    ERROR("CDistributionServer::readDistributionInfo(const std::vector<int>& nGlobal)",
       << "At least one dimension must be defined for this field.");
  }
  nGlobal_ = nGlobal;
  this->dims_ = nGlobal.size();
}

void CDistributionServer::createGlobalIndex()
{
  size_t globalIndexSize = 1;
  for (int i = 0; i < nGlobal_.size(); ++i)
    globalIndexSize *= nGlobal_[i];
  size_t rangeSize    = globalIndexSize / nServer_;
  size_t modulusSize = globalIndexSize % nServer_;

  if ((this->rank_ == (nServer_-1)) && (0 != modulusSize))
  {
    this->globalIndex_ = new CArray<size_t,1>(modulusSize);
    globalIndexSize = modulusSize;
  }
  else
  {
    this->globalIndex_ = new CArray<size_t,1>(rangeSize);
    globalIndexSize = rangeSize;
  }

  size_t idxBegin = this->rank_ * rangeSize;
  for (size_t i = 0; i < globalIndexSize;++i)
    (*this->globalIndex_)(i) = i+idxBegin;
}

}
