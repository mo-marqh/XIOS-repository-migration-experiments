#ifndef __XIOS_CLIENT_ONLINE_READER_FILTER__
#define __XIOS_CLIENT_ONLINE_READER_FILTER__

#include <map>

#include "output_pin.hpp"
#include "event_server.hpp"
#include "context_client.hpp"
#include "calendar_util.hpp"


namespace xios
{
  class CGrid;
  class CField;
  class CGridRedistributeFilterIn ;
  class CFileReaderSourceFilter ;

  /*!
   * A source filter is the entry point of the data in the graph of filters.
   */
  class CClientOnlineReaderFilter : public CFilter, public IFilterEngine, public std::enable_shared_from_this<CClientOnlineReaderFilter>
  {
    public:
      /*!
       * Constructs a source filter accepting data attached to the specified grid.
       *
       * \param gc the garbage collector associated with this filter
       * \param field associated to the filter
       */
      CClientOnlineReaderFilter(CGarbageCollector& gc, CField* field);
      virtual void connectOutput(std::shared_ptr<CInputPin> inputPin, size_t inputSlot) ;
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
      bool sendReadDataRequest(const CDate& tsDataRequested) ;
      CDataPacketPtr virtual apply(std::vector<CDataPacketPtr> data);
      bool sendReadDataRequestIfNeeded(void) ;
      void checkForLateData(void) ;

     private:
      
      CDuration freqOp_ ;
      CDuration offset_ ;
      CDate lastDateReceived_ ;
      bool isFirstDataSent_ = false ;

      CField* field_;
      shared_ptr<CGridRedistributeFilterIn> redistributeFilter_ ;
      std::shared_ptr<CFileReaderSourceFilter> fileReaderSourceFilter_ ; 

      bool isEOF_ = false ;
      CDate dateEOF_ ;
  }; // class CClientFromServerSourceFilter
} // namespace xios

#endif // __XIOS_CLIENT_ONLINE_READER_FILTER__