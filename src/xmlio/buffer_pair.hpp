#ifndef __XMLIO_CBufferPair__
#define __XMLIO_CBufferPair__

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
      class CBufferPair
      {

            /// Définition de type ///
            typedef CLinearBuffer BufferType;

         public :

            /// Construteurs ///
            CBufferPair(MPI_Comm com_client_server = CMPIManager::GetCommWorld());
            CBufferPair(const CBufferPair & buffer_pair);       // Not implemented yet.
            CBufferPair(const CBufferPair * const buffer_pair); // Not implemented yet.

            template <typename A1, typename A2, typename A3, typename A4>
               void prepareRequest(const long int & managerId,
                                   const long int & methodId,
                                   A1 * arg1 = CLinearBuffer::NULL_ARG,
                                   A2 * arg2 = CLinearBuffer::NULL_ARG,
                                   A3 * arg3 = CLinearBuffer::NULL_ARG,
                                   A4 * arg4 = CLinearBuffer::NULL_ARG);
            /// Accesseur ///    
            CLinearBuffer & getCurrentBuffer(void);
            
            /// Traitements divers ///
            MPI_Request wait(void);
            void sendCurrentBuffer(void); 
                        
            /// Destructeur ///
            virtual ~CBufferPair(void);
            
         protected :
            
            /// Test ///
            bool mustBeSent(void);           

         private :

            /// Propriétés privées ///
            MPI_Comm com_client_server;
            CLinearBuffer first, second;
            MPI_Request first_request, second_request;
            int currentBuffer;

      }; // class CBufferPair
      
      ///--------------------------------------------------------------
      
      template <typename A1, typename A2, typename A3, typename A4>
         void CBufferPair::prepareRequest(const long int & managerId,
                                          const long int & methodId,
                                          A1 * arg1, A2 * arg2, A3 * arg3, A4 * arg4)
      {       
         if (this->mustBeSent()) 
            this->sendCurrentBuffer();
           
         CLinearBuffer & cbuffer = this->getCurrentBuffer();
         StdSize usize = cbuffer.getUnusedSize();         
         StdSize rsize = cbuffer.getRequestedSize(arg1, arg2, arg3, arg4) ;
         StdSize msize = cbuffer.getSize();
         
         long int nbargs = 0;
         
         if (arg1 != NULL) nbargs++;
         if (arg2 != NULL) nbargs++;
         if (arg3 != NULL) nbargs++;
         if (arg4 != NULL) nbargs++;
         
         if (rsize > msize)
            ERROR("CBufferPair::sendRequest(...)", << "Le tampon client est trop petit !");
                  
         if (rsize <= usize)
         {
            cbuffer.appendRequestInfos(managerId, methodId, nbargs);
         }
         else
         {
            this->wait();
            this->prepareRequest(managerId, methodId, arg1, arg2, arg3, arg4);
         }
      }
      
      ///--------------------------------------------------------------
      
      
   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CBufferPair__
