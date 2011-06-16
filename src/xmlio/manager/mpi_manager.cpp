#include "mpi_manager.hpp"

#include "impi_interface.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Définitions ////////////////////// ///

      void CMPIManager::Initialise(int * UNUSED(argc), char *** UNUSED(argv))
      {
         int error = 0;
         bool flag = false;

         mpi_initialized(&flag, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::Initialise(arc, argv)", << " MPI Error !");
         if (!flag)
         {
            mpi_init(&error);
            if (error != mpi_success)
               ERROR("CMPIManager::Initialise(arc, argv)", << " MPI Error !");
         }
      }

      void CMPIManager::Finalize(void)
      {
         int error = 0;
         mpi_finalize(&error);
         if (error != mpi_success)
            ERROR("CMPIManager::Initialise(arc, argv)", << " MPI Error !");
      }

      ///--------------------------------------------------------------

      int CMPIManager::GetCommRank(MPIComm comm)
      {
         int rank = 0, error = 0;
         mpi_comm_rank(&comm, &rank, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::GetCommRank(comm)", << " MPI Error !");
         return (rank);
      }

      int CMPIManager::GetCommSize(MPIComm comm)
      {
         int size = 0, error = 0;
         mpi_comm_size(&comm, &size, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::GetCommSize(comm)", << " MPI Error !");
         return (size);
      }

      MPIComm CMPIManager::GetCommWorld(void)
      { 
         return (mpi_comm_world); 
      }

      bool CMPIManager::IsMaster(MPIComm comm)
      { 
         return (CMPIManager::GetCommRank(comm) == 0); 
      }

      bool CMPIManager::IsRank(MPIComm comm, int rank)
      { 
         return (CMPIManager::GetCommRank(comm) == rank); 
      }

      MPIComm CMPIManager::CreateComm(MPIGroup group, MPIComm pcomm)
      {
         MPIComm  commu = 0;
         int error = 0;
         mpi_comm_create(&pcomm, &group, &commu, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::CreateComm(group, pcomm)", << " MPI Error !");
         return (commu);
      }

      //---------------------------------------------------------------

      void CMPIManager::Barrier(MPIComm comm)
      {
         int error = 0;
         mpi_barrier(&comm, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::Barrier(comm)", << " MPI Error !");
      }

      //---------------------------------------------------------------

      MPIGroup CMPIManager::GetGroupWorld(void)
      {
         MPIGroup group = 0;
         int error = 0;
         MPIComm  commu = CMPIManager::GetCommWorld();
         mpi_comm_group(&commu, &group, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::GetGroupWorld()", << " MPI Error !");
         return (group);
      }

      MPIGroup CMPIManager::CreateSubGroup(MPIGroup pgroup, const std::vector<int> & ranks)
      {
         MPIGroup group = 0;
         int size = ranks.size();
         int error = 0;
         mpi_group_incl(&pgroup, &size, &(ranks[0]), &group, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::CreateSubGroup(pgroup, ranks)", << " MPI Error !");
         return (group);
      }

      MPIGroup CMPIManager::CreateSubGroup(MPIGroup pgroup, int min_rank, int max_rank, int intval)
      {
         std::vector<int> ranks;
         for (int i = min_rank; i <= max_rank; i += intval)
            ranks.push_back(i);
         return (CMPIManager::CreateSubGroup(pgroup, ranks));
      }

      //---------------------------------------------------------------

      void CMPIManager::AllocMem(void * data, StdSize size)
      {
         if (MPI_Alloc_mem(sizeof(char) * size, MPI_INFO_NULL, data) != MPI_SUCCESS)
            ERROR("CMPIManager::AllocMem(data, size)", << " MPI Error !");
      }

      void CMPIManager::FreeMem(void * data)
      { 
         MPI_Free_mem(data);
      }

      //--------------------------------------------------------------

      void CMPIManager::Send (MPIComm comm, int dest_rank, char * data,
                              StdSize size, MPIRequest & request)
      {
         MPIDataType type = mpi_char;
         int nsize = size;
         int tag = 0, error = 0;
         mpi_issend(data, &nsize, &type, &dest_rank, &tag, &comm, &request, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::Send (comm, dest_rank, data, size, request)", << " MPI Error !");
      }

      void CMPIManager::Wait (MPIRequest & request)
      {
         MPIStatus status = new int[mpi_status_size]();
         int error = 0;
         mpi_wait(&request, status, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::Wait (request)", << " MPI Error !");
         delete [] status;
      }

      bool CMPIManager::Test (MPIRequest & request)
      {
         MPIStatus status = new int[mpi_status_size]();
         bool flag = false;
         int error = 0;
         mpi_test(&request, &flag, status, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::Test (request)", << " MPI Error !");
         delete [] status;
         return (flag);
      }

      bool CMPIManager::HasReceivedData(MPIComm comm, int src_rank)
      {
         MPIStatus status = new int[mpi_status_size]();
         bool flag = false;
         int error = 0, tag = mpi_any_tag;
         mpi_iprobe(&src_rank, &tag, &comm, &flag, status, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::hasReceivedData (comm, rank)", << " MPI Error !");
         delete [] status;
         return (flag);
      }

      StdSize CMPIManager::GetReceivedDataSize(MPIComm comm, int src_rank)
      {
         MPIDataType type = mpi_char;
         MPIStatus status = new int[mpi_status_size]();
         bool flag = false;
         int error = 0, size = 0, tag = mpi_any_tag;

         mpi_iprobe(&src_rank, &tag, &comm, &flag, status, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::getReceivedDataSize (comm, rank)", << " MPI Error !");
         if (flag == false) return (0);        
         mpi_get_count(status, &type, &size, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::getReceivedDataSize (comm, rank)", << " MPI Error !");
         delete [] status;
         return (size);
      }

      void CMPIManager::Receive(MPIComm comm, int src_rank, char * data)
      {
         MPIRequest req = 0;
         MPIDataType type = mpi_char;
         int error = 0, tag = mpi_any_tag;
         int size = CMPIManager::GetReceivedDataSize(comm, src_rank);

         mpi_irecv(data, &size, &type, &src_rank, &tag, &comm, &req, &error);
         if (error != mpi_success)
            ERROR("CMPIManager::Receive (comm, src_rank, data)", << " MPI Error !");
         CMPIManager::Wait (req); // Temporaire
      }

      //--------------------------------------------------------------

      void CMPIManager::SendLinearBuffer
         (MPIComm comm, int dest_rank, CLinearBuffer & buff, MPIRequest & request)
      {
         CMPIManager::Send(comm, dest_rank, buff, buff.getUsedSize(), request);
         buff.clear();
      }

      void CMPIManager::ReceiveLinearBuffer(MPIComm comm, int src_rank, CLinearBuffer & buff)
      {
         CMPIManager::Receive(comm, src_rank, buff);
         buff.computeBufferData();
      }

      boost::shared_ptr<CLinearBuffer> CMPIManager::ReceiveLinearBuffer(MPIComm comm, int src_rank)
      {
         boost::shared_ptr<CLinearBuffer> buff_ptr
            (new CLinearBuffer(CMPIManager::GetReceivedDataSize(comm, src_rank)));
         CMPIManager::ReceiveLinearBuffer(comm, src_rank, *buff_ptr);
         return (buff_ptr);
      }

      void CMPIManager::ReceiveCircularBuffer(MPIComm comm, int src_rank, CCircularBuffer & buff)
      {
         StdSize data_size  = CMPIManager::GetReceivedDataSize(comm, src_rank);
         StdSize data_begin = buff.prepareNextDataPosition(data_size);
         CMPIManager::Receive(comm, src_rank, buff.getData(data_begin));
         buff.updateNbRequests(data_begin, data_begin + data_size);
      }

      ///--------------------------------------------------------------

   } // namespace comm
} // namespace xmlioserver
