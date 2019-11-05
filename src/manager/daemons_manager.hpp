#ifndef __DAEMONS_MANAGER_HPP__
#define __DAEMONS_MANAGER_HPP__

namespace xios
{


  class CDaemonsManager
  {
    public:

    CDaemonsManager(bool isXiosServer) ;
    ~CDaemonsManager() ;

    bool eventLoop(void) ;
    bool servicesEventLoop(void) ;
    
    private:
    bool isServer_ ;
  } ;
}

#endif