#ifndef __XIOS_CLIENT_FROM_SERVER_SOURCE_FILTER__
#define __XIOS_CLIENT_FROM_SERVER_SOURCE_FILTER__

#include <map>

#include "output_pin.hpp"
#include "event_server.hpp"
#include "calendar_util.hpp"


namespace xios
{
  class CGrid;
  class CField;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CClientFromServerSourceFilter : public COutputPin
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param field associated to the filter
       */
      CClientFromServerSourceFilter(CGarbageCollector& gc, CField* field);

      /*!
       * Transforms the data received from the model into a packet and send it
       * in the filter graph. The array containing the data can safely be reused
       * immediately after this method returns.
       *
       * \param event the event coming from server with the associated data
       */
      void streamData(CEventServer& event);
      bool isDataLate(void) ;
      bool isEOF() {return isEOF_ ;}

     private:
      CGrid* grid_;             //!< The grid attached to the data the filter can accept
      CDuration freqOp_ ;
      CDuration offset_ ;

      bool wasDataAlreadyReceived_= false ;
      CDate lastDateReceived_ ;
      bool isEOF_ = false ;
      CDate dateEOF_ ;
  }; // class CClientFromServerSourceFilter
} // namespace xios

#endif //__XIOS_CLIENT_FROM_SERVER_SOURCE_FILTER__