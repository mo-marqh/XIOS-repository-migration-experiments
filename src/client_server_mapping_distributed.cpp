/*!
   \file client_server_mapping.hpp
   \author Ha NGUYEN
   \since 27 Feb 2015
   \date 16 Mars 2016

   \brief Mapping between index client and server.
   Clients pre-calculate all information of server distribution.
 */
#include "client_server_mapping_distributed.hpp"
#include <limits>
#include <boost/functional/hash.hpp>
#include "utils.hpp"
#include "mpi_tag.hpp"

namespace xios
{

CClientServerMappingDistributed::CClientServerMappingDistributed(const boost::unordered_map<size_t,int>& globalIndexOfServer,
                                                                 const MPI_Comm& clientIntraComm, bool isDataDistributed)
  : CClientServerMapping(), ccDHT_(0)
{
  ccDHT_ = new CClientClientDHTInt(globalIndexOfServer,
                                   clientIntraComm,
                                   isDataDistributed);
}

CClientServerMappingDistributed::~CClientServerMappingDistributed()
{
  if (0 != ccDHT_) delete ccDHT_;
}

/*!
   Compute mapping global index of server which client sends to.
   \param [in] globalIndexOnClient global index client has
*/
void CClientServerMappingDistributed::computeServerIndexMapping(const CArray<size_t,1>& globalIndexOnClient)
{
  ccDHT_->computeIndexInfoMapping(globalIndexOnClient);
  const boost::unordered_map<size_t,int>& infoIndexMap = (ccDHT_->getInfoIndexMap());
//  indexGlobalOnServer_ = (ccDHT_->getInfoIndexMap());
  boost::unordered_map<size_t,int>::const_iterator it = infoIndexMap.begin(), ite = infoIndexMap.end();
  for (; it != ite; ++it)
  {
    indexGlobalOnServer_[it->second].push_back(it->first);
  }
}

}
