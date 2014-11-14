#ifndef __XIOS_HPP__
#define __XIOS_HPP__

#include "xmlioserver_spl.hpp"
#include "mpi.hpp"

namespace xios
{
  /*!
  \class CXios
  */
  class CXios
  {
    public:
     static void initialize(void) ;
     static void initClientSide(const string & codeId, MPI_Comm& localComm, MPI_Comm& returnComm) ;
     static void initServerSide(void) ;
     static void clientFinalize(void) ;
     static void parseFile(const string& filename) ;

     template <typename T>
     static T getin(const string& id,const T& defaultValue) ;

     template <typename T>
     static T getin(const string& id) ;

    public:
     static string rootFile ; //!< Configuration filename
     static string xiosCodeId ; //!< Identity for XIOS
     static string clientFile; //!< Filename template for client
     static string serverFile; //!< Filename template for server

     static bool isClient ; //!< Check if xios is client
     static bool isServer ; //!< Check if xios is server

     static MPI_Comm globalComm ; //!< Global communicator

     static bool printInfo2File; //!< Printing out information into file
     static bool usingOasis ; //!< Using Oasis
     static bool usingServer ; //!< Using server (server mode)
     static double bufferServerFactorSize ; //!< Factor helps tune buffer size
     static double defaultBufferServerFactorSize ; //!< Default factor value
     static bool isOptPerformance; //!< Check if buffer size is for performance (as large as possible)

    public:
     //! Setting xios to use server mode
     static void setUsingServer();

     //! Setting xios NOT to use server mode
     static void setNotUsingServer();

      //! A silly variable to detect whether one process is in client or server side. Should be removed on refactoring code
     static bool isServerSide;

     //! Initialize server (if any)
     static  void initServer();

    private:
      //! Parse only Xios part of configuration file
      static void parseXiosConfig();
  } ;
}

//#include "cxios_impl.hpp"
#endif // __XIOS_HPP__
