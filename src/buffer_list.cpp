#include "buffer_list.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CBufferList::CBufferList(MPI_Comm com_client_server)
         : SuperClass(CMPIManager::GetCommSize(com_client_server) - 1)
         , com_client_server(com_client_server)
         , nbbuffer(CMPIManager::GetCommSize(com_client_server) - 1)
      { /* Ne rien faire de plus */ }

      CBufferList::~CBufferList(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      bool CBufferList::hasRequests(void) const
      {
         bool retvalue = SuperClass::operator[](0).hasRequest();
         for (StdSize i = 1; i < this->nbbuffer; i++)
            retvalue = retvalue && SuperClass::operator[](i).hasRequest();
         return (retvalue);
      }

      bool CBufferList::recvRequests(void)
      {
         while (!this->hasRequests())
         {
            for (int i = 1; i < CMPIManager::GetCommSize(com_client_server); i++)
            {
               if (CMPIManager::HasReceivedData(com_client_server, i))
               {
                  StdSize size = CMPIManager::GetReceivedDataSize(com_client_server, i);
                  if (SuperClass::operator[](i-1).isAvailable(size))
                  {
                     CMPIManager::ReceiveCircularBuffer
                        (com_client_server, i, SuperClass::operator[](i-1));
                  }
                  else
                  {
                     DEBUG ("Impossible d'écrire dans le tampon " << i 
                             << " (Taille requise : " << size << " octets,"
                             << " Taille du buffer circulaire : "
                             << SuperClass::operator[](i-1).getSize() << " octets)");
                  }
               }
            }
         }
         

            
         return (true);
      }

      void CBufferList::getRequests(std::vector<CLinearBuffer> & lbuffer)
      {
         for (StdSize i = 0; i < this->nbbuffer; i++)
         {
            lbuffer.push_back(SuperClass::operator[](i).getNextRequest());
         }
      }

      StdSize CBufferList::getNumberOfBuffers(void) const
      { 
         return (this->nbbuffer) ;
      }

      ///--------------------------------------------------------------

   } // namespace tree
} // namespace xmlioserver

