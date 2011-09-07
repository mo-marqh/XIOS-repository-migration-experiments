/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */
 
 /**
 * \file    mpi_interface.cpp
 * \brief   Gestion des communications MPI via une surcouche interne (implémentation).
 * \author  Hervé Ozdoba
 * \version 0.4
 * \date    28 Juin 2011
 */
 
// XMLIOServer headers
#include "mpi_manager.hpp"


// /////////////////////////////// Définitions ////////////////////////////// //

namespace xmlioserver {
namespace comm {

   // ---------------------- Initialisation & Finalisation ---------------------

   void CMPIManager::Initialise(int * _argc, char *** _argv)
   {
      int flag = 0;
      if (MPI_Initialized(&flag) != MPI_SUCCESS)
         ERROR("CMPIManager::Initialise(arc, argv)", << " MPI Error !");
      if (!flag)
      {
         if (MPI_Init(_argc, _argv) != MPI_SUCCESS)
            ERROR("CMPIManager::Initialise(arc, argv)", << " MPI Error !");
      }

   }
   
   void CMPIManager::Finalize(void)
   {
      if (MPI_Finalize() != MPI_SUCCESS)
         ERROR("CMPIManager::Finalize(void)", << " MPI Error !");
   }
   
   // ------------------------------ Communicateurs ----------------------------
   
   int CMPIManager::GetCommRank(MPI_Comm _comm)
   {
      int rank = 0;
      if (MPI_Comm_rank(_comm, &rank) != MPI_SUCCESS)
         ERROR("CMPIManager::GetCommRank(comm)", << " MPI Error !");
      return (rank);
   }
   
   int CMPIManager::GetCommSize(MPI_Comm _comm)
   {
      int size = 0;
      if (MPI_Comm_size(_comm, &size) != MPI_SUCCESS)
         ERROR("CMPIManager::GetCommSize(comm)", << " MPI Error !");
      return (size);
   }
   
   MPI_Comm CMPIManager::CreateComm(MPI_Group _group, MPI_Comm _pcomm)
   {
      MPI_Comm commu;      
      if (MPI_Comm_create(_pcomm, _group, &commu) != MPI_SUCCESS)
         ERROR("CMPIManager::CreateComm(group, pcomm)", << " MPI Error !");
      return (commu);
   }
   
   //MPI_Comm CMPIManager::GetCommWorld(void)
   //{
   //   return (MPI_COMM_WORLD); 
   //}
   
   // ---------------------------------- Autre ---------------------------------
         
   void CMPIManager::Barrier(MPI_Comm _comm)
   {
      if (MPI_Barrier(_comm) != MPI_SUCCESS)
         ERROR("CMPIManager::Barrier(comm)", << " MPI Error !");
   }
   
   bool CMPIManager::DispatchClient(bool       _is_server,
                                    MPI_Comm & _comm_client,
                                    MPI_Comm & _comm_client_server,
                                    MPI_Comm & _comm_server,
                                    MPI_Comm   _comm_parent)
   {
      int value = (_is_server) ? 1 : 2;
      std::size_t nbClient = 0, nbServer = 0, nbClientByServer = 0;
      std::vector<int> info, rank_client, rank_server;
      CMPIManager::AllGather(value, info, _comm_parent);

      for (std::size_t s = 0;  s < info.size(); s++)
      {
         if (info[s] == 1) rank_server.push_back(s);
         else rank_client.push_back(s);
      }
      nbClient = rank_client.size();
      nbServer = rank_server.size();
      
      if (nbClient == 0)
         ERROR("CMPIManager::DispatchClient()", << " Aucun client disponible !");
         

      _comm_client = CMPIManager::CreateComm(CMPIManager::CreateSubGroup(
                     CMPIManager::GetGroupWorld(), rank_client), _comm_parent);

      if (nbServer != 0)
      {
         std::size_t currentServer = 0;
         nbClientByServer = nbClient/nbServer;
         _comm_server = CMPIManager::CreateComm(CMPIManager::CreateSubGroup(
                        CMPIManager::GetGroupWorld(), rank_server), _comm_parent);

         //std::cout << nbClient << "," << nbServer  << "," << nbClientByServer << std::endl;

         for (std::size_t mm = 0; mm < nbClient; mm += nbClientByServer)
         {
            std::vector<int> group_rank;
            group_rank.push_back(rank_server[currentServer++]);
            for (std::size_t nn = 0; nn < nbClientByServer; nn++)
               group_rank.push_back(rank_client[nn+mm]);
            MPI_Comm comm_client_server_ = CMPIManager::CreateComm(CMPIManager::CreateSubGroup(
                                           CMPIManager::GetGroupWorld(), group_rank), _comm_parent);

            if (std::find(group_rank.begin(), group_rank.end(),
                         CMPIManager::GetCommRank(_comm_parent)) != group_rank.end())
            {
               _comm_client_server = comm_client_server_;
            }
               
            group_rank.clear();
         }
         return (true);
      }
      else
      {
         _comm_server = _comm_client;
         return (false);
      }
   }
   

   // --------------------------------- Groupes --------------------------------
         
   MPI_Group CMPIManager::GetGroupWorld(void)
   {
      MPI_Group group = 0;
      if (MPI_Comm_group(MPI_COMM_WORLD, &group) != MPI_SUCCESS)
         ERROR("CMPIManager::GetGroupWorld()", << " MPI Error !");
      return (group);
   }
   
   MPI_Group CMPIManager::CreateSubGroup(MPI_Group _pgroup, const std::vector<int> & _ranks)
   {
      MPI_Group group = 0;
      if (MPI_Group_incl(_pgroup, _ranks.size(), const_cast<int*>(&(_ranks[0])), &group) != MPI_SUCCESS)
         ERROR("CMPIManager::CreateSubGroup(pgroup, ranks)", << " MPI Error !");
      return (group);
   }
   
   MPI_Group CMPIManager::CreateSubGroup
      (MPI_Group _pgroup, int _min_rank, int _max_rank, int _intval)
   {
      std::vector<int> ranks;
      for (int i = _min_rank; i <= _max_rank; i += _intval)
         ranks.push_back(i);
      return (CMPIManager::CreateSubGroup(_pgroup, ranks));
   }

   // ----------------------------------- Tests --------------------------------
         
   bool CMPIManager::IsMaster(MPI_Comm _comm)
   {
      return (CMPIManager::GetCommRank(_comm) == 0); 
   }
   
   bool CMPIManager::IsRank(int _rank, MPI_Comm _comm)
   {
      return (CMPIManager::GetCommRank(_comm) == _rank); 
   }

   // --------------------------- Communication simple -------------------------
         
   void CMPIManager::Send (MPI_Comm _comm, int _dest_rank, char * _data,
                           std::size_t _size, MPI_Request & _request)
   {
      int nsize = _size;    
      if (MPI_Issend(_data, nsize, MPI_CHAR, _dest_rank, 0, _comm, &_request) != MPI_SUCCESS)
         ERROR("CMPIManager::Send (comm, dest_rank, data, size, request)", << " MPI Error !");
   }
   
   void CMPIManager::Wait (MPI_Request & _request)
   {
      MPI_Status status;
      if (MPI_Wait(&_request, &status) != MPI_SUCCESS)
         ERROR("CMPIManager::Wait (request)", << " MPI Error !");
   }
   
   bool CMPIManager::Test (MPI_Request & _request)
   {
      MPI_Status status;
      int flag = 0;
      if (MPI_Test(&_request, &flag, &status) != MPI_SUCCESS)
         ERROR("CMPIManager::Test (request)", << " MPI Error !");
      return (flag);
   }

   bool CMPIManager::HasReceivedData(MPI_Comm _comm, int _src_rank)
   {
      MPI_Status status;
      int flag = 0;
      if (MPI_Iprobe(_src_rank, MPI_ANY_TAG, _comm, &flag, &status) != MPI_SUCCESS)
         ERROR("CMPIManager::HasReceivedData (comm, rank)", << " MPI Error !");
      return (flag);
   }
   
   std::size_t CMPIManager::GetReceivedDataSize(MPI_Comm _comm, int _src_rank)
   {
      MPI_Status status;
      int flag = 0, size = 0;
      if (MPI_Iprobe(_src_rank, MPI_ANY_TAG, _comm, &flag, &status) != MPI_SUCCESS)
         ERROR("CMPIManager::getReceivedDataSize (comm, rank)", << " MPI Error !");
      if (!flag) return (0);  
      if (MPI_Get_count(&status, MPI_CHAR, &size) != MPI_SUCCESS)
         ERROR("CMPIManager::getReceivedDataSize (comm, rank)", << " MPI Error !");

      return (size);
   }
   
   void CMPIManager::Receive(MPI_Comm _comm, int _src_rank, char * _data)
   {
      MPI_Request request = 0;
      int size = CMPIManager::GetReceivedDataSize(_comm, _src_rank);
      if (MPI_Irecv(_data, size, MPI_CHAR, _src_rank, MPI_ANY_TAG, _comm, &request) != MPI_SUCCESS)
         ERROR("CMPIManager::Receive (comm, src_rank, data)", << " MPI Error !");
      CMPIManager::Wait (request); // Temporaire
   }
   
   void CMPIManager::AllGather(int _indata, std::vector<int> & _outdata, MPI_Comm _comm)
   {
      std::vector<int> data; data.push_back(_indata);
      CMPIManager::AllGather(data, _outdata, _comm);
   }

   void  CMPIManager::AllGather(const std::vector<int> & _indata,
                                      std::vector<int> & _outdata, MPI_Comm _comm)
   {
      int sendcount = _indata.size(),
          recvcount = _indata.size() * CMPIManager::GetCommSize(_comm);
      _outdata.resize(recvcount);     
      if (MPI_Allgather ( const_cast<int*>(&(_indata[0])), sendcount, MPI_INTEGER,
                                          &(_outdata[0]) , recvcount, MPI_INTEGER, _comm) != MPI_SUCCESS)
         ERROR("CMPIManager::AllGather (indata, outdata, comm)", << " MPI Error !");
   }
         
   // ------------------------- Communication 'complexe' -----------------------
         
   void CMPIManager::SendLinearBuffer(MPI_Comm _comm, int _dest_rank, CLinearBuffer & _lbuffer, MPI_Request & _request)
   {
      CMPIManager::Send(_comm, _dest_rank, _lbuffer, _lbuffer.getUsedSize(), _request);
      _lbuffer.clear();
   }
   
   void CMPIManager::ReceiveLinearBuffer(MPI_Comm _comm, int _src_rank, CLinearBuffer & _lbuffer)
   {
      CMPIManager::Receive(_comm, _src_rank, _lbuffer);
      _lbuffer.computeBufferData();
   }
   
   boost::shared_ptr<CLinearBuffer> CMPIManager::ReceiveLinearBuffer(MPI_Comm _comm, int _src_rank)
   {
      boost::shared_ptr<CLinearBuffer> buff_ptr
         (new CLinearBuffer(CMPIManager::GetReceivedDataSize(_comm, _src_rank)));
      CMPIManager::ReceiveLinearBuffer(_comm, _src_rank, *buff_ptr);
      return (buff_ptr);
   }
   
   void CMPIManager::ReceiveCircularBuffer(MPI_Comm _comm, int _src_rank, CCircularBuffer & _cbuffer)
   {
      std::size_t data_size  = CMPIManager::GetReceivedDataSize(_comm, _src_rank);
      std::size_t data_begin = _cbuffer.prepareNextDataPosition(data_size);
      CMPIManager::Receive(_comm, _src_rank, _cbuffer.getData(data_begin));
      _cbuffer.updateNbRequests(data_begin, data_begin + data_size);
   }

   // ---------------------- Mémoire (non fonctionnel ....) --------------------
         
   void CMPIManager::AllocMemory(void * _data, std::size_t _size)
   {
      if (MPI_Alloc_mem(sizeof(char) * _size, MPI_INFO_NULL, _data) != MPI_SUCCESS)
         ERROR("CMPIManager::AllocMem(data, size)", << " MPI Error !");
   }
   
   void CMPIManager::FreeMemory (void * _data)
   {
      MPI_Free_mem(_data);
   }

} // namespace comm
} // namespace xmlioserver

