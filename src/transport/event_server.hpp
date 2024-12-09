#ifndef __EVENT_SERVER_HPP__
#define __EVENT_SERVER_HPP__

#include "xios_spl.hpp"
#include "buffer_in.hpp"
#include "buffer_server.hpp"

namespace xios
{
  class CContextServer ;

  class CEventServer
  {
    public:
    
    int classId ;
    int type ;
    int nbSender ;

    CEventServer(CContextServer* contextServer) : contextServer_(contextServer) {}

    void push(int rank,CServerBuffer* serverBuffer ,char* startBuffer,int size) ;
    int getNbSender(void) {return nbSender ;}

    CContextServer* getContextServer(void) { return contextServer_ ;}
    
    struct SSubEvent
    {
      int rank ;
      char* startBuffer ;
      CServerBuffer* serverBuffer ;
      CBufferIn*  buffer ;
      int size ;
    } ;
    
    list<SSubEvent> subEvents ;    
    
    bool isFull(void) ;
    ~CEventServer() ; 
    private :

    CContextServer* contextServer_ ;
  } ;

}

#endif
