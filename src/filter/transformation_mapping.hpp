/*!
   \file transformation_mapping.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 09 June 2015

   \brief Take charge of communication among clients to exchange transformed data.
 */
#ifndef __XIOS_TRANSFORMATION_MAPPING_HPP__
#define __XIOS_TRANSFORMATION_MAPPING_HPP__

#include <map>
#include <set>
#include "grid.hpp"
#include "array_new.hpp"
#include "client_server_mapping_distributed.hpp"

namespace xios {

/*!
  \class CTransformationMapping
  This class is in charge of transfer the global index form grid from grid source to grid destination in a rather generic way.
In order to make a transformation, the grid destination will make a request to grid source about the global indexes which it needs.
The same discovering algorithm as the case of client-server is applied to find the corresponding client which contains the demanding global index.
*/
class CTransformationMapping
{
public:
  /** Default constructor */
  CTransformationMapping(CGrid* destination, CGrid* source);
  ~CTransformationMapping();

  void computeTransformationMapping(const std::map<size_t, std::set<size_t> >& globaIndexMapFromDestToSource);
  const std::map<int,std::vector<std::vector<size_t> > >& getGlobalIndexReceivedOnGridDestMapping() const;
  const std::map<int,std::vector<size_t> >& getGlobalIndexSendToGridDestMapping() const;

protected:
  CGrid* gridSource_;  // Grid source
  CGrid* gridDestination_; // Grid destination demande the necessary global index from grid source

  //! Global index mapping of grid source and grid destination between two clients
  CClientServerMappingDistributed* gridIndexClientClientMapping_;

  //! Mapping of client rank of grid source and global index received in grid destination
  std::map<int,std::vector<std::vector<size_t> > > globalIndexReceivedOnGridDestMapping_;

  //! Mapping of client rank of grid destination and global index to send from grid source
  std::map<int,std::vector<size_t> > globalIndexSendToGridDestMapping_;

};

}
#endif // __XIOS_TRANSFORMATION_MAPPING_HPP__
