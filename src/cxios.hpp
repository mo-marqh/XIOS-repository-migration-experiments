#ifndef __XIOS_HPP__
#define __XIOS_HPP__

#include "xmlioserver_spl.hpp"
#include <mpi.h>

namespace xmlioserver
{
  class CXios
  {
     public:
     
     static string rootFile ;
     static string xiosCodeId ;
     
     static void initialize(void) ;
      
   
     static void initClientSide(const string & codeId, MPI_Comm& localComm, MPI_Comm& returnComm) ;
     static void initServerSide(void) ;
     static void clientFinalize(void) ;
   
     template <typename T>
     static T getin(const string& id) ;


     static bool isClient ;
     static bool isServer ;

     static MPI_Comm globalComm ; 
     
     static bool usingOasis ;
     static bool usingServer ;   
        
  } ;
}

#include "cxios_impl.hpp"









#endif
