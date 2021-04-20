#ifndef __DAEMONS_MANAGER_HPP__
#define __DAEMONS_MANAGER_HPP__
#include <cstddef>

namespace xios
{


  class CDaemonsManager
  {
    public:

    CDaemonsManager(bool isXiosServer) ;
    ~CDaemonsManager() ;

    bool eventLoop(void) ;
    bool servicesEventLoop(void) ;
    
    void scheduleContext(size_t hashId) { scheduledContext_=hashId ;} //!< for attached mode, give the hand to the associated context server
    bool isScheduledContext(size_t hashId) { return scheduledContext_==hashId ;} //!< for attached mode, return true if context server is sceduled
    void unscheduleContext(void) { scheduledContext_=0 ;} //!< for attached mode : unschedule context

    private:
    bool isServer_ ;
    size_t scheduledContext_ = 0 ; //!< Hash id of the next scehduled context for attached mode
  } ;
}

#endif