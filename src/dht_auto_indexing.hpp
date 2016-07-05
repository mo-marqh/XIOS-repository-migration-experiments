/*!
   \file dht_auto_indexing.hpp
   \author Ha NGUYEN
   \since 6 Jul 2016
   \date 6 Jul 2016

   \brief Auto assign global index across processes.
 */

#ifndef __XIOS_DHT_AUTO_INDEXING_HPP__
#define __XIOS_DHT_AUTO_INDEXING_HPP__

#include "client_client_dht_template.hpp"

namespace xios
{

/*!
  \class CDHTAutoIndexing
  .
*/
class CDHTAutoIndexing: public CClientClientDHTTemplate<size_t>
{
public:
  CDHTAutoIndexing(const CArray<size_t,1>& hashValue,
                   const MPI_Comm& clientIntraComm);

    /** Default destructor */
    virtual ~CDHTAutoIndexing();
};

} // namespace xios
#endif // __XIOS_DHT_AUTO_INDEXING_HPP__
