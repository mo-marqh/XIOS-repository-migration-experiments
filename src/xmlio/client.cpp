#include "client.hpp"

#include "linear_buffer_impl.hpp"

namespace xmlioserver {
namespace comm {

   /// ////////////////////// Définitions ////////////////////// ///
   
   CClient::CClient(MPIComm comm_client_server)
      : bpair(comm_client_server)
   { 
      this->initialize();
   }  
        
   CClient::~CClient(void)
   { 
      this->finalize();
   } 
   
   ///--------------------------------------------------------------

   boost::shared_ptr<CClient> CClient::Client;

   ///--------------------------------------------------------------

   boost::shared_ptr<CClient> CClient::CreateClient(MPIComm comm_client_server)
   {
      if (CClient::Client.get() != NULL)
         CClient::Client = boost::shared_ptr<CClient>(new CClient(comm_client_server));
      return (CClient::GetClient());
   }

   ///--------------------------------------------------------------

   boost::shared_ptr<CClient> CClient::GetClient(void)
   {
      return (CClient::Client);
   }

   ///--------------------------------------------------------------
   
   const CBufferPair & CClient::getBufferPair(void) const 
   {
      return (this->bpair);
   }   
   
   //---------------------------------------------------------------

   void CClient::initialize(void)
   {
      this->bpair.prepareRequest(0, 0, CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG);
      // Pas d'argument à ajouter.
   }
   
   //---------------------------------------------------------------
   
   void CClient::finalize(void)
   {
      this->bpair.prepareRequest(0, 1, CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG);
      // Pas d'argument à ajouter.
      this->bpair.sendCurrentBuffer();
      comm::CMPIManager::Barrier();
   }
   
   //---------------------------------------------------------------
   
   void CClient::setContext(const StdString & idContext)
   {
      this->bpair.prepareRequest(1, 0, &idContext,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG);
      this->bpair.getCurrentBuffer().appendString(idContext);
   }
   
   //---------------------------------------------------------------
   
   void CClient::updateCalendar(long int timestep)
   {
      this->bpair.prepareRequest(1, 1, &timestep,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG);
      this->bpair.getCurrentBuffer().appendInt(timestep);
   }

   //---------------------------------------------------------------

   void CClient::setTimestep(const date::CDuration & duration)
   {
      StdString durationstr = duration.toString();
      this->bpair.prepareRequest(1, 2, &durationstr,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG,
                                       CLinearBuffer::NULL_ARG);
      this->bpair.getCurrentBuffer().appendString(durationstr);
   }
   
   //---------------------------------------------------------------
   
   void CClient::sendData
      (const StdString & fieldId, const StdString & fileId, const ARRAY(float, 1) dataArray)
   {
      this->bpair.prepareRequest(2, 0, &fieldId, &fileId, &dataArray,
                                       CLinearBuffer::NULL_ARG);
      this->bpair.getCurrentBuffer().appendString(fieldId);
      this->bpair.getCurrentBuffer().appendString(fileId);
      this->bpair.getCurrentBuffer().appendFloatArray(dataArray);
   }
   
   //---------------------------------------------------------------
   
   void CClient::sendData
      (const StdString & fieldId, const StdString & fileId, const ARRAY(double, 1) dataArray)
   {
      this->bpair.prepareRequest(2, 1, &fieldId, &fileId, &dataArray,
                                       CLinearBuffer::NULL_ARG);
      this->bpair.getCurrentBuffer().appendString(fieldId);
      this->bpair.getCurrentBuffer().appendString(fileId);
      this->bpair.getCurrentBuffer().appendDoubleArray(dataArray);
   }

   ///--------------------------------------------------------------

} // namespace comm
} // namespace xmlioserver
