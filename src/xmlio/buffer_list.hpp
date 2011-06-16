#ifndef __XMLIO_CBufferList__
#define __XMLIO_CBufferList__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "buffer.hpp"
#include "circular_buffer.hpp"
#include "linear_buffer.hpp"
#include "mpi_manager.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CBufferList
         : public std::vector<CCircularBuffer>   // << pas une liste
      {

            /// Définition de type ///
            typedef std::vector<CCircularBuffer> SuperClass;
            typedef CCircularBuffer BufferType;

         public :

            /// Construteurs ///
            CBufferList(MPIComm com_client_server = CMPIManager::GetCommWorld());
            CBufferList(const CBufferList & buffer_list);         // Not implemented yet.
            CBufferList(const CBufferList * const buffer_list);   // Not implemented yet.

            /// Test ///
            bool hasRequests(void) const;

            /// Traitement ///
            bool recvRequests(void);

            /// Accesseurs ///
            StdSize getNumberOfBuffers(void) const;
            void getRequests(std::vector<CLinearBuffer> & lbuffer);

            /// Destructeur ///
            virtual ~CBufferList(void);


         private :

            MPIComm com_client_server;
            StdSize nbbuffer;

      }; // class CBufferList
   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CBufferList__
