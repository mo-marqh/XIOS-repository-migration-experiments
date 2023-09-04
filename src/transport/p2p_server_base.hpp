#ifndef __P2P_SERVER_BASE_HPP__
#define __P2P_SERVER_BASE_HPP__
#include "xios_spl.hpp"

namespace xios
{

  class CP2pServerBuffer ;
  
  class CP2pServerBase
  {
    public:
      struct SPendingEvent
      {
        int nbSenders ;
        int currentNbSenders ;
        std::list<CP2pServerBuffer*> buffers ;
      } ;
  } ;

}




#endif