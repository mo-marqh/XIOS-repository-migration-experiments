#ifndef __XMLIO_Server__
#define __XMLIO_Server__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "mpi_manager.hpp"
#include "buffer_list.hpp"
#include "array.hpp"

namespace xmlioserver {
namespace comm {

   /// ////////////////////// Déclarations ////////////////////// ///
   
   class CServer
   {
      public :

         static boost::shared_ptr<CServer> CreateServer(MPIComm comm_client_server);
         static boost::shared_ptr<CServer> GetServer(void);

         /// Destructeur ///
         ~CServer(void);
         
         void run(void);
         
      protected : 
      
         /// Accesseur ///
         const CBufferList & getBufferList(void) const;
         
      public :
      
         /// Appels distants///   
         void initialize(void); // manager 0, method 0
         void finalize(void);   // manager 0, method 1
             
         void setContext    (const std::vector<CLinearBuffer> & buffer); // manager 1, method 0
         void updateCalendar(const std::vector<CLinearBuffer> & buffer); // manager 1, method 1
         void setTimestep   (const std::vector<CLinearBuffer> & buffer); // manager 1, method 2
         
         void writeData(const std::vector<CLinearBuffer> & buffer, int prec); // manager 2, method 0 - 1
      
      private :

         /// Constructeurs ///
         CServer(MPIComm comm_client_server);
      
         /// Propriété privée ///
         CBufferList blist;

         static boost::shared_ptr<CServer> Server;
      
   }; // class CServer  

} // namespace comm
} // namespace xmlioserver

#endif //__XMLIO_Server__
