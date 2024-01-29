#ifndef __P2P_CS_BUFFER_BASE_HPP__
#define __P2P_CS_BUFFER_BASE_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include "event_client.hpp"
#include <limits>

namespace xios
{
  class CP2pCSBufferBase 
  {
    protected :

      const int controlSize_ = 2 ;
      const int CONTROL_ADDR = 0 ;
      const int CONTROL_FINALIZE = 1  ;

      const size_t EVENT_BUFFER_RESIZE = std::numeric_limits<size_t>::max()-1 ;
      const int MAX_WINDOWS=60 ;


  } ;

}


#endif
