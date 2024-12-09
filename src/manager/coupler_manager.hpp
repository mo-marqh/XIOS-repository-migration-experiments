#ifndef __COUPLER_MANAGER_HPP__
#define __COUPLER_MANAGER_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

#include <string>
#include <map>
#include "window_manager.hpp"


namespace xios
{
  // the rule of this manager is to schedule intercommunicator creation between two or more coupling context
  
  class CCouplerManager
  {
  
  public:
    CCouplerManager(bool isXiosServer);
    ~CCouplerManager() ;    
    void registerCoupling(std::string srcCoupling, std::string dstCoupling, bool master) ;
    bool isNextCoupling(string srcCoupling, string dstCoupling) ;
    void eventLoop(void) ;
  private :
    void registredCouplingDumpOut(CBufferOut& buffer) ;
    void registredCouplingDumpIn(CBufferIn& buffer) ;
    void nextCouplingDumpOut(CBufferOut& buffer);
    void nextCouplingDumpIn(CBufferIn& buffer);
    
    const std::string getStrCoupling(std::string srcCoupling, std::string dstCoupling) { return std::string("__"+srcCoupling+"__to__"+dstCoupling+"__") ; }
    /** windows containing a set of coupling (hash) registred by at least one context */
    CWindowManager* winRegistredCoupling_ ; 
 
    /** windows containing a list of coupling fully registred by two context. The first coupling of the list will be the next candidate for intercommunicator creation */
    CWindowManager* winNextCoupling_ ;

    /** size of the windows, ie around 1024 coupling context can be registred at the time */
    const size_t maxBufferSize_=1024*8 ;

    std::set<size_t> registredCoupling_ ;
    std::list<pair<size_t,int>> nextCoupling_ ;

    std::set<size_t> masterRegistred_ ;
    std::hash<string> hashString ;

    size_t hashRegistredCoupling_ ;
    size_t hashNextCoupling_ ;
    bool useWindowManager_ ;
    size_t hashCurrentRegistredCoupling_;
    size_t hashCurrentNextCoupling_;
    /** the rank of the mpi proc where the data are kept in memory*/
    int managerGlobalLeader_ ;
  } ;


}

#endif