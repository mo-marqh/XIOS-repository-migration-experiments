#include "server.hpp"

#include "tree_manager.hpp"
#include "duration.hpp"
#include "data_treatment.hpp"
#include "group_template_impl.hpp"

namespace xmlioserver {
namespace comm {

   /// ////////////////////// Définitions ////////////////////// ///
   
   CServer::CServer(MPIComm comm_client_server)
      : blist(comm_client_server)
   { /* Ne rien faire de plus */ }  
        
   CServer::~CServer(void)
   { /* Ne rien faire de plus */ } 
   
   ///--------------------------------------------------------------

   boost::shared_ptr<CServer> CServer::Server;

   ///--------------------------------------------------------------
   
   const CBufferList & CServer::getBufferList(void) const 
   {
      return (this->blist);
   }

   ///--------------------------------------------------------------

   boost::shared_ptr<CServer> CServer::CreateServer(MPIComm comm_client_server)
   {
      if (CServer::Server.get() == NULL)
         CServer::Server = boost::shared_ptr<CServer>(new CServer(comm_client_server));
      return (CServer::GetServer());
   }

   ///--------------------------------------------------------------

   boost::shared_ptr<CServer> CServer::GetServer(void)
   {
      return (CServer::Server);
   }
   
   //---------------------------------------------------------------
   
   void CServer::run(void)
   {
      while (this->blist.recvRequests())
      {
         long int managerId = -1, methodId  = -1, nbargs  = -1;
         long int managerId_= -1, methodId_ = -1, nbargs_ = -1;
         std::vector<CLinearBuffer> lbuffer;
         this->blist.getRequests(lbuffer);
                 
         for (StdSize i = 0; i < lbuffer.size(); i++)
         {           
            lbuffer[i].getRequestInfos(0, managerId, methodId, nbargs);
            if (((managerId_ != managerId) || (methodId_ != methodId) || (nbargs_ != nbargs)) && (i != 0 ))
            {
               //std::cout << managerId_ << "<->" << managerId << std::endl
               //          << methodId_  << "<->" << methodId  << std::endl
               //          << nbargs_    << "<->" << nbargs    << std::endl;
               ERROR("CServer::run(void)", << "[" << i << "] Les requêtes ne sont pas synchronisées !");
            }
            managerId_ = managerId;
            methodId_  = methodId;
            nbargs_    = nbargs;
         }
            
         if (managerId == 0)
         {
            switch(methodId)
            {
               case (0) :
                  this->initialize();
                  continue;
               case (1) :
                  this->finalize();
                  return;
               default  :
                  ERROR("CServer::run(void)", 
                     << "[ managerId = " << managerId << ", "
                     << "[ methodId = "  << methodId  << ", "
                     << "[ nbargs = "    << nbargs    << "] "
                     << " Methode inconnue !");
            }
         }
         if (managerId == 1)
         {
            switch(methodId)
            {
               case (0) :
                  this->setContext(lbuffer);
                  continue;
               case (1) :
                  this->updateCalendar(lbuffer);
                  continue;
               case (2) :
                  this->setTimestep(lbuffer);
                  continue;
               default  :
                  ERROR("CServer::run(void)", 
                     << "[ managerId = " << managerId << ", "
                     << "[ methodId = "  << methodId  << ", "
                     << "[ nbargs = "    << nbargs    << "] "
                     << " Methode inconnue !");
            }
         }
         if (managerId == 2)
         {
            switch(methodId)
            {
               case (0) :
                  this->writeData(lbuffer, 4);
                  continue;
               case (1) :
                  this->writeData(lbuffer, 8);
                  continue;
               default  :
                  ERROR("CServer::run(void)", 
                     << "[ managerId = " << managerId << ", "
                     << "[ methodId = "  << methodId  << ", "
                     << "[ nbargs = "    << nbargs    << "] "
                     << " Methode inconnue !");
            }
         }
         ERROR("CServer::run(void)", 
                << "[ managerId = " << managerId << ", "
                << "[ methodId = "  << methodId  << ", "
                << "[ nbargs = "    << nbargs    << "] "
                << " Methode inconnue !");
      }
   }

   //--------------------------------------------------------------
   
   void CServer::initialize(void) // manager 0, method 0
   {
      std::cout << "initialize" << std::endl;
   }
   
   //--------------------------------------------------------------
   
   void CServer::finalize(void) // manager 0, method 1
   {
      std::cout << "finalize" << std::endl;
       comm::CMPIManager::Barrier();
   }   

   //--------------------------------------------------------------

   void CServer::setTimestep(const std::vector<CLinearBuffer> & buffer)// manager 1, method 2
   {
      boost::shared_ptr<CContext> context =
         CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId());
      boost::shared_ptr<data::CDataTreatment> dtreat = context->getDataTreatment();

      StdString durationstr;
      for (StdSize i = 0; i < buffer.size(); i++)
      {
         StdString durationstr_ = buffer[i].getString(3);
         if (durationstr.compare(durationstr_) != 0 && (i != 0))
         {
            ERROR("CServer::setTimestep(const std::vector<CLinearBuffer> & buffer)",
                  << "[ durationstr 1 = " << durationstr   << ", "
                  << "[ durationstr 2 = " << durationstr_  << "] "
                  << " Modification de timestep désynchronisé !");
         }
         else durationstr = durationstr_;

      }
      std::cout << "setTimestep called " << durationstr << std::endl;
      dtreat->set_timestep(date::CDuration::FromString(durationstr));
   }

   //--------------------------------------------------------------
             
   void CServer::setContext(const std::vector<CLinearBuffer> & buffer) // manager 1, method 0
   { 
      StdString contextId;
      for (StdSize i = 0; i < buffer.size(); i++)
      {
         StdString contextId_ = buffer[i].getString(3);
         if (contextId.compare(contextId_) != 0 && (i != 0))
         {
            ERROR("CServer::setContext(const std::vector<CLinearBuffer> & buffer)", 
                  << "[ contextId 1 = " << contextId   << ", "
                  << "[ contextId 2 = " << contextId_  << "] "
                  << " Changement de contexte désynchronisé !");
         }
         else contextId = contextId_;
         
      }
      std::cout << "setContext called " << contextId << std::endl;
      CTreeManager::SetCurrentContextId(contextId);
   }
   
   //--------------------------------------------------------------
   
   void CServer::updateCalendar(const std::vector<CLinearBuffer> & buffer) // manager 1, method 1
   {
      boost::shared_ptr<CContext> context =
         CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId());
      boost::shared_ptr<data::CDataTreatment> dtreat = context->getDataTreatment();

      int timestep = -1;
      for (StdSize i = 0; i < buffer.size(); i++)
      {
         int timestep_ = buffer[i].getInt(3);
         if ((timestep_ != timestep)  && (i != 0))
         {
            ERROR("CServer::updateCalendar(const std::vector<CLinearBuffer> & buffer)", 
                  << "[ timestep 1 = " << timestep   << ", "
                  << "[ timestep 2 = " << timestep_  << "] "
                  << " Mise à jour du calendrier désynchronisée !");
         }
         else timestep = timestep_;
      }
      std::cout << "updateCalendar called " << timestep <<std::endl;
      dtreat->update_calendar(timestep);
   }
   
   //--------------------------------------------------------------
         
   void CServer::writeData(const std::vector<CLinearBuffer> & buffer, int prec)  // manager 2, method 0 - 1
   {
      boost::shared_ptr<CContext> context =
         CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId());
      boost::shared_ptr<data::CDataTreatment> dtreat = context->getDataTreatment();

      StdString fieldId, fileId;
      for (StdSize i = 0; i < buffer.size(); i++)
      {
         StdString fieldId_ = buffer[i].getString(3);
         StdString fileId_ = buffer[i].getString(4);

         if (fieldId.compare(fieldId_) != 0 && (i != 0))
         {           
            ERROR("CServer::writeData(const std::vector<CLinearBuffer> & buffer, int prec)", 
                  << "[fieldId 1 = " << fieldId   << ", "
                  << " fieldId 2 = " << fieldId_  << "] "
                  << " Ecriture des données désynchronisée !");
         }
         else fieldId = fieldId_;

         if (fileId.compare(fileId_) != 0 && (i != 0))
         {
            ERROR("CServer::writeData(const std::vector<CLinearBuffer> & buffer, int prec)",
                  << "[fileId 1 = " << fileId   << ", "
                  << " fileId 2 = " << fileId_  << "] "
                  << " Ecriture des données désynchronisée !");
         }
         else fileId = fileId_;
      }
      
      if (prec == 4)
      {
         std::deque<ARRAY(float, 1)> dataArray(buffer.size());
         for (StdSize i = 0; i < buffer.size(); i++)
            dataArray[i] = buffer[i].getFloatArray(5);
         std::cout << "writeData called (float) " << fieldId << ", " << dataArray[0] << std::endl;
         // Jamais atteint car les données sont transférées en tant que double
         return;
      }      
      else if (prec == 8)
      {
         std::deque<ARRAY(double, 1)> dataArray(buffer.size());
         for (StdSize i = 0; i < buffer.size(); i++)
            dataArray[i] = buffer[i].getDoubleArray(5);
         std::cout << "writeData called (double) " << fieldId << ", " << dataArray[0]  << std::endl;
         dtreat->write_data(fieldId, fileId, dataArray);
         return;
      }      
      else
      {
         ERROR("CServer::writeData(const std::vector<CLinearBuffer> & buffer, int prec)", 
               << " Précision des données invalide !");
      }
   }
  
   ///--------------------------------------------------------------
   
   
} // namespace comm
} // namespace xmlioserver
