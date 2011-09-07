#include "buffer_pair.hpp"

#include "mpi_manager.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CBufferPair::CBufferPair(MPIComm com_client_server)
         : com_client_server(com_client_server) 
         , first(BUFFER_CLIENT_SIZE), second(BUFFER_CLIENT_SIZE)
         , first_request(MPI_REQUEST_NULL), second_request(MPI_REQUEST_NULL)
         , currentBuffer(0)
      { /* Ne rien faire de plus */ }

      CBufferPair::~CBufferPair(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      bool CBufferPair::mustBeSent(void)
      {
         if ((currentBuffer  ==  0) && (first.getUsedSize()  != 0) &&
            ((second_request == MPI_REQUEST_NULL) || CMPIManager::Test (second_request)))
             return (true);
             
         if ((currentBuffer  ==  1) && (second.getUsedSize() != 0) &&
            ((first_request  == MPI_REQUEST_NULL) || CMPIManager::Test (first_request)))
            return (true);
            
         return (false);
      }
      
      //---------------------------------------------------------------
      
      MPI_Request CBufferPair::wait(void)
      {
         if (this->currentBuffer == 0)
         {
            CMPIManager::Wait(this->second_request);
            this->second_request = MPI_REQUEST_NULL;
            return (this->second_request);
         }
         else
         {
            CMPIManager::Wait(this->first_request);
            this->first_request = MPI_REQUEST_NULL;
            return (this->first_request);
         }
      }
      
      //---------------------------------------------------------------
      
      void CBufferPair::sendCurrentBuffer(void)
      {
         if (this->currentBuffer == 0)
         {
            CMPIManager::SendLinearBuffer
               (this->com_client_server, 0, this->first, this->first_request);
            this->currentBuffer  =  1;
            this->second_request = MPI_REQUEST_NULL;
            this->second.clear();
         }
         else  if(this->currentBuffer == 1)
         {
            CMPIManager::SendLinearBuffer
               (this->com_client_server, 0, this->second, this->second_request);
            this->currentBuffer =  0;
            this->first_request = MPI_REQUEST_NULL;
            this->first.clear();
         }
      }
      
      //---------------------------------------------------------------
      
      CLinearBuffer & CBufferPair::getCurrentBuffer(void)
      {
         if (currentBuffer == 0) return (this->first);
         return (this->second);
      }
      
      ///--------------------------------------------------------------

   } // namespace tree
} // namespace xmlioserver

