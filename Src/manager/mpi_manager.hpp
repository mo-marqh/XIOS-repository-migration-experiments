/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#ifndef __MPI_INTERFACE_HPP__
#define __MPI_INTERFACE_HPP__

/**
 * \file    mpi_interface.hpp
 * \brief   Gestion des communications MPI via une surcouche interne (entête).
 * \author  Hervé Ozdoba
 * \version 0.4
 * \date    28 Juin 2011
 */

#ifndef __XIOS_NO_EXTERN

// M(essage) P(assing) I(nterface) headers
#include <mpi.h>

// C++ standard headers
#include <vector>

#endif //__XIOS_NO_EXTERN

// XMLIOServer headers
#include "xmlioserver_spl.hpp"
#include "buffer.hpp"
#include "circular_buffer.hpp"
#include "linear_buffer.hpp"

// ////////////////////////////// Déclarations ///////////////////////////// //

namespace xmlioserver
{
   /// \brief Espace de nommage pour les communications via la bibliothèque MPI.
   namespace comm
   {
      typedef MPI_Fint     MPIComm; /*!< \brief Identifiant de communicateur MPI (Fortran).   */
      typedef MPI_Fint    MPIGroup; /*!< \brief Identifiant de groupe MPI (Fortran).          */
      typedef MPI_Fint  MPIRequest; /*!< \brief Identifiant de requête MPI (Fortran).         */
      typedef MPI_Fint  *MPIStatus; /*!< \brief Identifiant de statut MPI (Fortran).          */
      typedef MPI_Fint MPIDataType; /*!< \brief Identifiant de type de données MPI (Fortran). */
      
      /**
       * \class CMPIManager
       * \brief Surcouche interne de la bibliàothèque M(essage) P(assing) I(nterface).
       */
      class CMPIManager
      {
         public : // Initialisation & Finalisation
         
            static void Initialise(int * argc, char *** argv);
            static void InitialiseClient(int * argc, char *** argv);
            static void InitialiseServer(int * argc, char *** argv);
            static void Finalize(void);

         public : // Communicateurs
         
         
           
            static inline MPI_Comm GetCommWorld(void)            
            { return (MPI_COMM_WORLD); }
            
            static int GetCommRank(MPI_Comm _comm = MPI_COMM_WORLD);
            static int GetCommSize(MPI_Comm _comm = MPI_COMM_WORLD);       
            
            static MPI_Comm CreateComm(MPI_Group _group, MPI_Comm _pcomm=MPI_COMM_WORLD);

            static inline MPI_Comm GetCommClient(void) 
            { return CommClient; }   
            static inline MPI_Comm GetCommServer(void) 
            { return CommServer; }   
            static inline MPI_Comm GetCommClientServer(void)             
            { return CommClientServer; }   
            static inline int GetNbClient(void)             
            { return NbClient; }   
            static inline int GetNbServer(void)             
            { return NbServer; }   

            static inline bool IsConnected(void)             
            { return (NbServer==0)?true:false; }   

            static inline bool IsClient(void)             
            { return _IsClient; }   

            static inline bool IsServer(void)             
            { return _IsServer; }   

         public : // Autre
         
            static void Barrier(MPI_Comm _comm = MPI_COMM_WORLD);
            
            static bool DispatchClient(bool       _is_server,
                                       MPI_Comm & _comm_client,
                                       MPI_Comm & _comm_client_server,
                                       MPI_Comm & _comm_server,
                                       MPI_Comm _comm_parent);

         public : // Groupes
         
            static MPI_Group GetGroupWorld(void);
            static MPI_Group GetGroup(MPI_Comm Comm);
            static MPI_Group CreateSubGroup(MPI_Group _pgroup, const std::vector<int> & _ranks);
            static MPI_Group CreateSubGroup(MPI_Group _pgroup, int _min_rank, int _max_rank, int _intval = 1);

         public : // Tests 
         
       
         
            static bool IsMaster(MPI_Comm _comm = MPI_COMM_WORLD);
            static bool IsRank(int _rank, MPI_Comm _comm = MPI_COMM_WORLD);

         public : // Communication simple
         
            static void Send (MPI_Comm _comm, int _dest_rank, char * _data,
                              std::size_t _size, MPI_Request & _request);
            static void Wait (MPI_Request & _request);
            static bool Test (MPI_Request & _request);

            
            static bool HasReceivedData(MPI_Comm _comm, int _src_rank);
            
            static std::size_t GetReceivedDataSize(MPI_Comm _comm, int _src_rank);

            static void Receive(MPI_Comm _comm, int _src_rank, char * _data);
            
            static void AllGather(int _indata, std::vector<int> & _outdata,
                                  MPI_Comm _comm = MPI_COMM_WORLD);

            static void AllGather(const std::vector<int> & _indata,
                                        std::vector<int> & _outdata,
                                  MPI_Comm _comm = MPI_COMM_WORLD);


         public : // Communication 'complexe'
         

            static void SendLinearBuffer(MPI_Comm _comm, int _dest_rank, CLinearBuffer & _lbuffer, MPI_Request & _request);
            static void ReceiveLinearBuffer(MPI_Comm _comm, int _src_rank, CLinearBuffer & _lbuffer);
            static boost::shared_ptr<CLinearBuffer> ReceiveLinearBuffer(MPI_Comm _comm, int _src_rank);
            static void ReceiveCircularBuffer(MPI_Comm _comm, int _src_rank, CCircularBuffer & _cbuffer);
            
            

         public : // Mémoire (non fonctionnel ....)
         
            static void AllocMemory(void * _data, std::size_t _size);
            static void FreeMemory (void * _data);
         
          private :
          
            static bool Initialized ;
            static MPI_Comm CommClient ;
            static MPI_Comm CommServer ;
            static MPI_Comm CommClientServer ;
            static int NbClient ;
            static int NbServer ;

            static bool _IsClient ;
            static bool _IsServer ;
            static bool using_server ;
            static bool using_oasis ;
            
      }; // class CMPIManager
      

      
   } // namespace comm
} // namespace xmlioserver

#endif //__MPI_INTERFACE_HPP__
