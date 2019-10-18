#ifndef __DAEMONS_MANAGER_HPP__
#define __DAEMONS_MANAGER_HPP__

namespace xios
{


  class CDaemonsManager
  {
    public:

    CDaemonsManager(bool isXiosServer) ;

    bool eventLoop(void) ;
    
    private:
    bool isServer_ ;
  } ;
}

#endif