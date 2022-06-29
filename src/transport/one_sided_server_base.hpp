#ifndef __ONE_SIDED_SERVER_BASE_HPP__
#define __ONE_SIDED_SERVER_BASE_HPP__
#include "xios_spl.hpp"

namespace xios
{

  class COneSidedServerBuffer ;
  
  class COneSidedServerBase
  {
    public:
      struct SPendingEvent
      {
        int nbSenders ;
        int currentNbSenders ;
        std::list<COneSidedServerBuffer*> buffers ;
      } ;
  } ;

}




#endif