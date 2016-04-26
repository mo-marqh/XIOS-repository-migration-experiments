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
#include "axis.hpp"
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
  typedef boost::unordered_map<size_t, std::vector<std::pair<int, std::pair<size_t,double> > > > DestinationIndexMap;
  struct ReceivedIndex {
    ReceivedIndex(int l, size_t g, double w) : localIndex(l),globalIndex(g), weight(w) {}
    ReceivedIndex() : localIndex(0), globalIndex(0), weight(0.0) {}
    int localIndex;
    size_t globalIndex;
    double weight;
  };

  typedef boost::unordered_map<int,std::vector<ReceivedIndex> > ReceivedIndexMap;
  typedef boost::unordered_map<int,std::vector<std::pair<int, size_t> > > SentIndexMap;

public:
  /** Default constructor */
  CTransformationMapping(CGrid* destination, CGrid* source);
  CTransformationMapping(CAxis* destination, CAxis* source);

  ~CTransformationMapping();

  void computeTransformationMapping(const DestinationIndexMap& globaIndexMapFromDestToSource);
  const ReceivedIndexMap& getGlobalIndexReceivedOnGridDestMapping() const;
  const SentIndexMap& getGlobalIndexSendToGridDestMapping() const;

protected:
  CGrid* gridSource_;  // Grid source
  CGrid* gridDestination_; // Grid destination demande the necessary global index from grid source

  //! Global index mapping of grid source and grid destination between two clients
  CClientClientDHTPairIntInt* gridIndexClientClientMapping_;

  //! Mapping of client rank of grid source and global index received in grid destination
  ReceivedIndexMap globalIndexReceivedOnGridDestMapping_;

  //! Mapping of client rank of grid destination and global index to send from grid source
  SentIndexMap globalIndexSendToGridDestMapping_;
};

}
#endif // __XIOS_TRANSFORMATION_MAPPING_HPP__
