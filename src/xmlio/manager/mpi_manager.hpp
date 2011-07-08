#ifndef __XMLIO_CMPIManager__
#define __XMLIO_CMPIManager__

/// MPI headers ///
#include <mpi.h>

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer.hpp"
#include "circular_buffer.hpp"
#include "linear_buffer.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      typedef MPI_Fint     MPIComm;
      typedef MPI_Fint    MPIGroup;
      typedef MPI_Fint  MPIRequest;
      typedef MPI_Fint  *MPIStatus;
      typedef MPI_Fint MPIDataType;

      class CMPIManager
      {
         public :

            /// Initialisation & Finalisation ///
            static void Initialise(int * argc, char *** argv);
            static void Finalize(void);

            /// Communicateurs ///
            static int GetCommRank(MPIComm comm = CMPIManager::GetCommWorld());
            static int GetCommSize(MPIComm comm = CMPIManager::GetCommWorld());
            static MPIComm CreateComm(MPIGroup group, MPIComm pcomm = CMPIManager::GetCommWorld());
            static MPIComm GetCommWorld(void);

            /// Autre ///
            static void Barrier(MPIComm comm = CMPIManager::GetCommWorld());

            static bool DispatchClient(bool      is_server,
                                       MPIComm & comm_client,
                                       MPIComm & comm_client_server,
                                       MPIComm & comm_server,
                                       MPIComm   comm_parent = CMPIManager::GetCommWorld());

            /// Groupes ///
            static MPIGroup GetGroupWorld(void);
            static MPIGroup CreateSubGroup(MPIGroup pgroup, const std::vector<int> & ranks);
            static MPIGroup CreateSubGroup(MPIGroup pgroup, int min_rank, int max_rank, int intval = 1);

            /// Tests ///
            static bool IsMaster(MPIComm comm = CMPIManager::GetCommWorld());
            static bool IsRank(MPIComm comm, int rank);

            /// Communication simple///
            static void Send (MPIComm comm, int dest_rank, char * data, StdSize size, MPIRequest & request);
            static void Wait (MPIRequest & request);
            static bool Test (MPIRequest & request);

            static void AllGather(int indata, std::vector<int> & outdata,
                                  MPIComm comm = CMPIManager::GetCommWorld());

            static void AllGather(std::vector<int> & indata,
                                  std::vector<int> & outdata,
                                  MPIComm comm = CMPIManager::GetCommWorld());

            static bool HasReceivedData(MPIComm comm, int src_rank);
            static StdSize GetReceivedDataSize(MPIComm comm, int src_rank);
            static void Receive(MPIComm comm, int src_rank, char * data);

            /// Communication 'complexe' ///
            static void SendLinearBuffer(MPIComm comm, int dest_rank, CLinearBuffer & buff, MPIRequest & request);
            static void ReceiveLinearBuffer(MPIComm comm, int src_rank, CLinearBuffer & buff);
            static boost::shared_ptr<CLinearBuffer> ReceiveLinearBuffer(MPIComm comm, int src_rank);
            static void ReceiveCircularBuffer(MPIComm comm, int src_rank, CCircularBuffer & buff);

            /// Mémoire (non fonctionnel ....)///
            static void AllocMem(void * data, StdSize size);
            static void FreeMem(void * data);

      }; // class CMPIManager

   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CMPIManager__

