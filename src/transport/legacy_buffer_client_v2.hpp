#ifndef __LEGACY_BUFFER_CLIENT_V2_HPP__
#define __LEGACY_BUFFER_CLIENT_V2_HPP__

#include "buffer_cs_base.hpp"
#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include "window_dynamic_view.hpp"
#include "window_dynamic.hpp"

namespace xios
{
  class CLegacyClientBufferV2 : public CBufferClientServerBase
  {
    public:
      static size_t maxRequestSize;

      CLegacyClientBufferV2(MPI_Comm intercomm, int serverRank, StdSize bufferSize, bool hasWindows, CWindowDynamic* winDynamic_);
      ~CLegacyClientBufferV2();
//      void createWindows(MPI_Comm oneSidedComm) ;
      void freeWindows(void) ;
      void lockBuffer(void) ;
      void unlockBuffer(void) ;
      
      bool isBufferFree(StdSize size);
      CBufferOut* getBuffer(size_t timeLine, StdSize size);
      bool checkBuffer(bool send=false);
      bool hasPendingRequest(void);
      StdSize remain(void);
      MPI_Aint getWinAddress(int numWindows) ;
      MPI_Aint getWinBufferAddress(int numWindows) ;
      void infoBuffer(void) ;
      bool isNotifiedFinalized(void) ;
      void setGrowableBuffer(double growFactor) { growFactor_=growFactor ; isGrowableBuffer_=true ;}
      void fixBufferSize(size_t bufferSize) { newBufferSize_=bufferSize ; isGrowableBuffer_=false ; resizingBufferStep_=1 ;}
      void fixBuffer(void) { isGrowableBuffer_=false ;}
      void attachWindows() ;
      bool isAttachedWindows(void) { return isAttachedWindows_ ;}
    private:
       void resizeBuffer(size_t newSize) ;
       void resizeBufferNotify(void) ;
       bool isNotifiedChangeBufferSize(void) ;


      char* buffer[2];
      char* bufferHeader[2];
      size_t* firstTimeLine[2] ;
      size_t* bufferCount[2] ;
      size_t* control[2] ;
      size_t* notify[2] ;
      bool winState[2] ;
      int current;
      
      double growFactor_=1.2 ;
      bool isGrowableBuffer_=true ;

      int resizingBufferStep_ = 0 ;
      size_t newBufferSize_ ;
      StdSize count;
      StdSize maxEventSize;
      StdSize bufferSize;
      bool isFinalized_=false ;

      const int serverRank;
      int clientRank_;
      bool pending;

      MPI_Request request;

      CBufferOut* retBuffer;
      const MPI_Comm interComm;
      CWindowDynamic* winDynamic_ ;
      std::vector<CWindowDynamicView*> windows_ ;
      bool hasWindows_=false ;
      bool isAttachedWindows_=false ;
      double latency_=0 ;
      double lastCheckedWithNothing_=0 ;
      double lastCheckedNotify_=0 ;
  };
}
#endif
