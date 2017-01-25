#ifndef EP_TYPE_HPP_INCLUDED
#define EP_TYPE_HPP_INCLUDED


#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <list>
#include <map>
#include <omp.h>
#include <vector>
#include <numeric>
#include <bitset>
#include <memory.h>
#include <algorithm>
#include <assert.h>
#include <math.h>

#ifdef _Debug
#define Debug(x) std::cout << x << std::endl
#else
#define Debug(x)
#endif

#define BUFFER_SIZE 10000

typedef std::pair< int, int > SIZE_RANK_INFO; // < rank, size>

typedef std::vector< std::pair<int, int> > RANK_MAP;  // at(ep_rank) = <ep_rank_local, mpi_rank>

typedef std::vector<std::pair< std::pair<int, int>, std::pair<int, int> > > INTERCOMM_RANK_MAP;


typedef struct
{
  int first;
  int second;
  int third;
} Triple_int;

namespace ep_lib
{
  #define MPI_UNDEFINED -32766
  #define MPI_STATUS_IGNORE NULL
  #define MPI_INFO_NULL MPI_Info()

  class ep_communicator;
  class ep_intercomm;
  class OMPbarrier;
  typedef ep_communicator* EP_Comm;
  class MPI_Comm;


  class MPI_Status
  {
    public:

      #ifdef _intelmpi
      int ep_datatype;
      #elif _openmpi
      void * ep_datatype;
      #endif
      
      int ep_src;
      int ep_tag;

      
      void* mpi_status;
  };

  class MPI_Message
  {
    public:

      #ifdef _intelmpi
      int mpi_message;
      #elif _openmpi
      void * mpi_message;
      #endif

      int ep_src;
      int ep_tag;

      void* mpi_status;

      MPI_Message()
      {
        mpi_message = 0;
        mpi_status = 0;
      }
  };

  typedef std::list<MPI_Message > Message_list;


  class OMPbarrier
  {
    private:
      int nbThreads;          //<The number of threads for this barrier
      int currentNbThread;    //<The current number of threads waiting
      bool sense;             //<Direct barrier feedback protection
      omp_lock_t mutex;       //<To have an atomic int

      OMPbarrier(OMPbarrier&){}
      OMPbarrier& operator=(OMPbarrier&){return *this;}

    public:
      /** Constructor with the number of threads */
      explicit OMPbarrier(const int inNbThreads)
          : nbThreads(inNbThreads), currentNbThread(0), sense(false) {
          omp_init_lock( &mutex );
      }

      /** Destructor, release the omp lock */
      ~OMPbarrier(){
          omp_destroy_lock( &mutex );
      }

      /** Perform a barrier */
      void wait(){
          const bool mySense = sense;
          omp_set_lock( &mutex );
          const int nbThreadsArrived = (++currentNbThread);
          omp_unset_lock( &mutex );

          if(nbThreadsArrived == nbThreads) {
              currentNbThread = 0;
              sense = !sense;
              #pragma omp flush
          }
          else {
              volatile const bool* const ptSense = &sense;
              while( (*ptSense) == mySense){
              }
          }
      }


      /** Change the number of threads */
      void setNbThreads(const int inNbThread){
          omp_set_lock( &mutex );
          nbThreads = inNbThread;
          omp_unset_lock( &mutex );
      }
  };

  class ep_intercomm
  {
    public:


    #ifdef _intelmpi
    int mpi_inter_comm;
    #elif _openmpi
    void * mpi_inter_comm;
    #endif

    RANK_MAP *intercomm_rank_map;
    RANK_MAP *local_rank_map;
    RANK_MAP *remote_rank_map;


    SIZE_RANK_INFO size_rank_info[3];


    MPI_Comm *local_comm;
    int intercomm_tag;

    ep_intercomm()
    {
      intercomm_rank_map = NULL;
      local_rank_map = NULL;
      remote_rank_map = NULL;
    }

    bool operator == (ep_intercomm right)
    {
      bool a = intercomm_rank_map == right.intercomm_rank_map;
      bool b = local_rank_map == right.local_rank_map;
      bool c = remote_rank_map == right.remote_rank_map;
      bool d = mpi_inter_comm == right.mpi_inter_comm;
      bool e = size_rank_info == right.size_rank_info;
      bool f = intercomm_tag == right.intercomm_tag;
      return a&&b&&c&&d&&e&&f;
    }

    bool operator != (ep_intercomm right)
    {
      bool a = intercomm_rank_map != right.intercomm_rank_map;
      bool b = local_rank_map != right.local_rank_map;
      bool c = remote_rank_map != right.remote_rank_map;
      bool d = mpi_inter_comm != right.mpi_inter_comm;
      bool e = size_rank_info != right.size_rank_info;
      bool f = intercomm_tag != right.intercomm_tag;
      return a||b||c||d||e||f;
    }
  };


  class ep_communicator
  {
    public:

    SIZE_RANK_INFO size_rank_info[3]; // 0: ep_rank,     ep_size
                                      // 1: ep_rank_loc, num_ep
                                      // 2: mpi_rank,    mpi_size


    MPI_Comm *comm_list;

    Message_list *message_queue;


    int comm_label;

    ep_intercomm *intercomm;

    ep_communicator()
    {
      comm_list = NULL;
      message_queue = NULL;
      intercomm = NULL;
    }

    bool operator == (ep_communicator right)
    {
      bool a = size_rank_info == right.size_rank_info;
      bool b = comm_label == right.comm_label;
      bool c = intercomm == right.intercomm;
      return a&&b&&c;
    }

    bool operator != (ep_communicator right)
    {
      bool a = size_rank_info != right.size_rank_info;
      bool b = comm_label != right.comm_label;
      bool c = intercomm != right.intercomm;
      return a||b||c;
    }
  };


  struct BUFFER
  {
    double *buf_double;
    float  *buf_float;
    int    *buf_int;
    long    *buf_long;
    unsigned long    *buf_ulong;
    char    *buf_char;
  };


  class MPI_Comm
  {
    public:

    #ifdef _intelmpi
    int mpi_comm;
    #elif _openmpi
    void * mpi_comm;
    #endif

    bool is_ep;
    bool is_intercomm;

    BUFFER     *my_buffer;
    OMPbarrier *ep_barrier;
    RANK_MAP   *rank_map;

    EP_Comm ep_comm_ptr;

    MPI_Comm *mem_bridge;

    #ifdef _intelmpi
    int mpi_bridge;
    #elif _openmpi
    void * mpi_bridge;
    #endif

    MPI_Comm()
    {
      is_ep = false;
      is_intercomm = false;
      my_buffer = NULL;
      ep_barrier = NULL;
      rank_map = NULL;
      ep_comm_ptr = NULL;
      mem_bridge = NULL;
      mpi_bridge = NULL;
      mpi_comm = 0;
    }

    MPI_Comm(int comm)
    {
      is_ep = false;
      is_intercomm = false;
      my_buffer = NULL;
      ep_barrier = NULL;
      rank_map = NULL;
      ep_comm_ptr = NULL;
      mem_bridge = NULL;
      mpi_bridge = NULL;
      mpi_comm = comm;
    }

    //MPI_Comm(const MPI_Comm &comm);


    bool operator == (MPI_Comm right)
    {
      bool a = is_ep == right.is_ep;
      bool b = is_intercomm == right.is_intercomm;
      bool c = mpi_comm == right.mpi_comm;
      bool d = is_ep ? ep_comm_ptr == right.ep_comm_ptr : true;
      return a&&b&&c&&d;
    }

    bool operator != (MPI_Comm right)
    {
      bool a = is_ep != right.is_ep;
      bool b = is_intercomm != right.is_intercomm;
      bool c = mpi_comm != right.mpi_comm;
      bool d = is_ep ? ep_comm_ptr != right.ep_comm_ptr : true;
      return a||b||c||d;
    }
  };


  class MPI_Info
  {
    public:

      #ifdef _intelmpi
      int mpi_info;
      #elif _openmpi
      void * mpi_info;
      #endif

      MPI_Info()
      {
        mpi_info = 0;
      }
  };


  class MPI_Request
  {
    public:

      #ifdef _intelmpi
      int mpi_request;
      #elif _openmpi
      void * mpi_request;
      #endif

      int type;	//! type of the non-blocking communication. 1: Isend; 2:Irecv; 3:Imrecv; 4:Issend
      void* buf;

      int ep_src;
      int ep_tag;
      #ifdef _intelmpi
      int ep_datatype;
      #elif _openmpi
      void * ep_datatype;
      #endif

      MPI_Comm comm;	//! EP communicator related to the communication

      MPI_Request()
      {
        mpi_request = 0;
      }
  };

  class MPI_Fint
  {
    public:

    int mpi_fint;
  };

  class MPI_Aint
  {
    public:

    unsigned long mpi_aint;
  };


  static MPI_Comm *passage;

  static int TAG = 40000;

  static std::list<std::pair<std::pair<int, int>, MPI_Comm * > > tag_list;

  static std::map<std::pair<int, int>, MPI_Comm  >  fc_comm_map;
            //    <MPI_Fint,thread_num>   EP_Comm

}



#endif // EP_TYPE_HPP_INCLUDED
