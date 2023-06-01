#ifndef __XIOS_CLIENT_ONLINE_WRITER_FILTER__
#define __XIOS_CLIENT_ONLINE_WRITER_FILTER__

#include <map>

#include "output_pin.hpp"
#include "event_server.hpp"
#include "context_client.hpp"
#include "calendar_util.hpp"


namespace xios
{
  class CGrid;
  class CField;
  class CGridRedistributeFilter ;
  class CFileWriterStoreFilter ;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CClientOnlineWriterFilter : public CFilter, public IFilterEngine
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param field associated to the filter
       */
      CClientOnlineWriterFilter(CGarbageCollector& gc, CField* field);
      CDataPacketPtr apply(std::vector<CDataPacketPtr> data) ;
     private:

      std::shared_ptr<CGridRedistributeFilter> redistributeFilter_ ;
      std::shared_ptr<CFileWriterStoreFilter> fileWriterStoreFilter_ ; 
  }; // class CClientFromServerSourceFilter
} // namespace xios

#endif // __XIOS_ONLINE_WRITER_FILTER__