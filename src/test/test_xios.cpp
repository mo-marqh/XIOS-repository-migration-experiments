#include "xmlioserver.hpp"
#include "attribute_template_impl.hpp"
/*
#include "group_template_impl.hpp"
#include "file.hpp"
*/
#include "buffer_out.hpp"
#include "buffer_in.hpp"
#include "type.hpp"
#include "cxios.hpp"
#include "client_ym.hpp"
#include "event_client.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "object_template_impl.hpp"
#include <mpi.h>


using namespace std ;
using namespace ym ;

int main (int argc, char ** argv, char ** UNUSED (env))
{
  
  int a=3 ;
  int b=2 ;
  int c=1 ;
  int buff[100] ;
  string str("titi") ;
   CBufferOut bufferOut(&buff,sizeof(buff)) ;
   CBufferIn  bufferIn(&buff,sizeof(buff)) ;
   CMessage msg ;
/*
   msg<<(int)3<<(int)2<<(int)1<<str ;
//   msg<<a<<b<<c ;
   std::cout<<"message size : "<<msg.size()<<std::endl ;
   str="turlututu" ;
   bufferOut<<msg ;
   msg.clear() ;
   msg<<a<<b<<c<<str ;
//   bufferIn>>c>>b>>a>>str ;
   bufferIn>>msg ;
   std::cout<<a<<"  "<<b<<"  "<<c<<"  "<<str<<endl ;
*/
//   CAttributeTemplate<ARRAY(double,1)> tabIn ;
 //  CAttributeTemplate<ARRAY(double,1)> tabOut ;
   CAttributeTemplate<ARRAY(double,1)> tabIn("in") ;
   CAttributeTemplate<ARRAY(double,1)> tabOut("out") ;

   ARRAY_CREATE(tab,double,1,[5]) ; 
 //   tab->resize(extents[5]) ;
    (*tab)[0]=0 ; (*tab)[1]=1 ;(*tab)[2]=2 ; (*tab)[3]=3 ;(*tab)[4]=4 ;
    tabIn=tab ;
    (*tab)[0]=4 ; (*tab)[1]=3 ;(*tab)[2]=2 ; (*tab)[3]=1 ;(*tab)[4]=0 ;
    tabOut=tab ;
//   tabOut=1 ;
   tabIn.toBuffer(bufferOut) ;
   tabOut.fromBuffer(bufferIn) ;
   
   cout<<"attribut<Arraydouble,5>>="<<tabOut.toString()<<endl ;
   
   MPI_Init(&argc,&argv) ;
   int rank ;
   int size ;
   char buffer[128] ;
   
   MPI_Comm_rank(MPI_COMM_WORLD,&rank) ;
   if (rank>=0 && rank<=1) 
   {
     CXios::initClientSide("test1") ;
     ym::CClient::registerContext("toto",ym::CClient::intraComm) ;
//     ym::CClient::registerContext("tata",ym::CClient::intraComm) ;
     ym::CClient::registerContext("tutu",ym::CClient::intraComm) ;
     CObjectFactory::SetCurrentContextId("tutu") ;
     CContext*  tutu=(CObjectFactory::GetObject<CContext>("tutu")).get(); 
     
     
     CObjectFactory::SetCurrentContextId("toto") ;
     CContext*  toto=(CObjectFactory::GetObject<CContext>("toto")).get(); 
     toto->calendar_type.setValue("NoLeap") ;
     toto->sendAttributToServer("calendar_type" );


       for(int i=0;i<0;i++)
       {
         CMessage msg ;
         int count ;
         int msgSize; 
         CEventClient event(1,1) ;
         count=rand()%32 ;
         msg<<string("toto")<<string(buffer,count)<<rank<<msgSize;
         msgSize=msg.size() ;
         event.push(0,2,msg) ;
         event.push(1,2,msg) ;
         toto->client->sendEvent(event) ;
         cout<<"Send Event from toto : size "<<msgSize<<endl ;
       }
     toto->client->finalize() ;
     tutu->client->finalize() ;
     CXios::clientFinalize() ;
   }
   else if (rank>=2 && rank<=3) CXios::initServerSide();
   else if (rank>=4 && rank<=6) 
   {
     CXios::initClientSide("test2") ;
     ym::CClient::registerContext("tito",ym::CClient::intraComm) ;
     CObjectFactory::SetCurrentContextId("tito") ;
     CContext*  tito=(CObjectFactory::GetObject<CContext>("tito")).get();
      
     ym::CClient::registerContext("tete",ym::CClient::intraComm) ;
     CObjectFactory::SetCurrentContextId("tete") ;
     CContext*  tete=(CObjectFactory::GetObject<CContext>("tete")).get(); 

     tito->client->finalize() ;
     tete->client->finalize() ;
     CXios::clientFinalize() ;
   } 
   else if (rank>=7 && rank<=7) 
   {
     CXios::initClientSide("test3") ;
     ym::CClient::registerContext("turlututu",ym::CClient::intraComm) ;
     CObjectFactory::SetCurrentContextId("turlututu") ;
     CContext*  turlututu=(CObjectFactory::GetObject<CContext>("turlututu")).get(); 
     turlututu->client->finalize() ;
     CXios::clientFinalize() ;
   }
   
   
  
   MPI_Barrier(MPI_COMM_WORLD); 
   MPI_Finalize() ;

  return EXIT_SUCCESS ;
 
}
