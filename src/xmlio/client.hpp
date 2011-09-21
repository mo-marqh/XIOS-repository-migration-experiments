#ifndef __XMLIO_Client__
#define __XMLIO_Client__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "mpi_manager.hpp"
#include "buffer_pair.hpp"
#include "array.hpp"
#include "duration.hpp"

namespace xmlioserver {
namespace comm {

   /// ////////////////////// Déclarations ////////////////////// ///
   
   class CClient
   {
      public :

         static boost::shared_ptr<CClient> CreateClient(MPIComm comm_client_server);
         static boost::shared_ptr<CClient> GetClient(void);
                
         /// Destructeur ///
         ~CClient(void);  
         
      public : 
      
         /// Accesseur ///
         const CBufferPair & getBufferPair(void) const;
              
         /// Appels distants///   
         void initialize(void); // manager 0, method 0
         void finalize(void);   // manager 0, method 1
         
      public : 
       
         void setContext(const StdString & idContext);      // manager 1, method 0
         void updateCalendar(long int timestep);            // manager 1, method 1
         void setTimestep(const date::CDuration & duration);// manager 1, method 2
         
         void sendData(const StdString & fieldId,
                       const StdString & fileId,
                       const ARRAY(float, 1)  dataArray); // manager 2, method 0
         
         void sendData(const StdString & fieldId,
                       const StdString & fileId,
                       const ARRAY(double, 1) dataArray); // manager 2, method 1
         
      private :

         /// Constructeurs ///
         CClient(MPIComm comm_client_server);
      
         /// Propriété privée ///
         CBufferPair bpair;

         static boost::shared_ptr<CClient> Client;
      
   }; // class CClient   
   
} // namespace comm
} // namespace xmlioserver

#endif //__XMLIO_Client__
