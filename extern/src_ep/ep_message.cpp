/*!
   \file ep_message.cpp
   \since 2 may 2016

   \brief Definitions of MPI endpoint function: Message_Check
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;

namespace ep_lib
{

  int Message_Check(MPI_Comm comm)
  {
    int myRank;
    MPI_Comm_rank(comm, &myRank);

    //printf("myRank = %d, comm.is_ep = %d, comm.is_intercomm = %d\n", myRank, comm.is_ep, comm.is_intercomm);
    if(!comm.is_ep) return 0;

    if(comm.is_intercomm)
	  {
      return  Message_Check_intercomm(comm);
    }

    int flag = true;
    ::MPI_Message message;
    ::MPI_Status status;
    int mpi_source;

    while(flag) // loop until the end of global queue
    {
      Debug("Message probing for intracomm\n");
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm> (comm.mpi_comm);
      #ifdef _openmpi
      #pragma omp critical (_mpi_call)
      {
        ::MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpi_comm, &flag, &status);
        if(flag)
        {
          Debug("find message in mpi comm \n");
          mpi_source = status.MPI_SOURCE;
          int tag = status.MPI_TAG;
          ::MPI_Mprobe(mpi_source, tag, mpi_comm, &message, &status);

        }
      }
      #elif _intelmpi
      //::MPI_Improbe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpi_comm, &flag, &message, &status); 
      ::MPI_Improbe(-2, -1, mpi_comm, &flag, &message, &status); 
      //printf("myRank = %d, ::MPI_Improbe flag = %d\n", myRank, flag);     
      #endif

      if(flag)
      {

        MPI_Message *msg_block = new MPI_Message; //printf("myRank = %d, new ok\n", myRank);
        msg_block->mpi_message = message;  //printf("myRank = %d, msg_block->mpi_message = %d\n", myRank, msg_block->mpi_message);
        msg_block->ep_tag = bitset<15>(status.MPI_TAG >> 16).to_ulong();  //printf("myRank = %d, msg_block->ep_tag = %d\n", myRank, msg_block->ep_tag);
        int src_loc       = bitset<8> (status.MPI_TAG >> 8) .to_ulong();  //printf("myRank = %d, src_loc = %d\n",           myRank, src_loc);
        int dest_loc      = bitset<8> (status.MPI_TAG)	    .to_ulong();  //printf("myRank = %d, dest_loc = %d\n",          myRank, dest_loc);
        int src_mpi       = status.MPI_SOURCE;                            //printf("myRank = %d, src_mpi = %d\n",           myRank, src_mpi);
             
        msg_block->ep_src  = get_ep_rank(comm, src_loc,  src_mpi);        //printf("myRank = %d, msg_block->ep_src = %d\n",  myRank, msg_block->ep_src);

        //printf("myRank = %d, probed one message, ep_src = %d, ep_dest = %d, tag = %d, message = %d\n", myRank, msg_block->ep_src, msg_block->ep_dest, msg_block->ep_tag, msg_block->mpi_message);
        msg_block->mpi_status = new ::MPI_Status(status);

        MPI_Comm* ptr_comm_list = comm.ep_comm_ptr->comm_list;
        MPI_Comm* ptr_comm_target = &ptr_comm_list[dest_loc];


        #pragma omp critical (_query)
        {
          #pragma omp flush
          ptr_comm_target->ep_comm_ptr->message_queue->push_back(*msg_block);  
          // printf("myRank = %d, push_back OK, ep_src = %d, ep_tag = %d, mpi_status = %p (%p)\n", myRank, 
          //                                                                                   ptr_comm_target->ep_comm_ptr->message_queue->back().ep_src,
          //                                                                                   ptr_comm_target->ep_comm_ptr->message_queue->back().ep_tag,
          //                                                                                   msg_block->mpi_status, &status);
    
          #pragma omp flush
        }
        
        delete msg_block;
        //printf("myRank = %d, delete msg_block, queue size = %lu\n", myRank, ptr_comm_target->ep_comm_ptr->message_queue->size());
        
      }

    }

    return MPI_SUCCESS;
  }



  int Message_Check_intercomm(MPI_Comm comm)
  {
    if(!comm.ep_comm_ptr->intercomm->mpi_inter_comm) return 0;

    Debug("Message probing for intercomm\n");

    int flag = true;
    ::MPI_Message message;
    ::MPI_Status status;
    int mpi_source;
    int current_ep_rank;
    MPI_Comm_rank(comm, &current_ep_rank);

    while(flag) // loop until the end of global queue "comm.ep_comm_ptr->intercomm->mpi_inter_comm"
    {
      Debug("Message probing for intracomm\n");
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm> (comm.ep_comm_ptr->intercomm->mpi_inter_comm);  // => mpi_intercomm
      
      #ifdef _openmpi
      #pragma omp critical (_mpi_call)
      {
        ::MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpi_comm, &flag, &status);
        if(flag)
        {
          Debug("find message in mpi comm \n");
          mpi_source = status.MPI_SOURCE;
          int tag = status.MPI_TAG;
          ::MPI_Mprobe(mpi_source, tag, mpi_comm, &message, &status);

        }
      }
      #elif _intelmpi
      //::MPI_Improbe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpi_comm, &flag, &message, &status);       
      ::MPI_Improbe(-2, -1, mpi_comm, &flag, &message, &status);       
      #endif

      if(flag)
      {

        MPI_Message *msg_block = new MPI_Message;

        msg_block->mpi_message = message;
        msg_block->ep_tag = bitset<15>(status.MPI_TAG >> 16).to_ulong();
        int src_loc       = bitset<8> (status.MPI_TAG >> 8) .to_ulong();
        int dest_loc      = bitset<8> (status.MPI_TAG)	    .to_ulong();
        int src_mpi       = status.MPI_SOURCE;
        int current_inter = comm.ep_comm_ptr->intercomm->local_rank_map->at(current_ep_rank).first;
             
        msg_block->ep_src  = get_ep_rank_intercomm(comm, src_loc,  src_mpi);
        msg_block->mpi_status = new ::MPI_Status(status);


        MPI_Comm* ptr_comm_list = comm.ep_comm_ptr->comm_list;
        MPI_Comm* ptr_comm_target = &ptr_comm_list[dest_loc];


        #pragma omp critical (_query)
        {
          #pragma omp flush
          ptr_comm_target->ep_comm_ptr->message_queue->push_back(*msg_block);
          //printf("probed one message, ep_src = %d, ep_dest = %d, tag = %d, queue = %p, message = %d\n", msg_block->ep_src, msg_block->ep_dest, msg_block->ep_tag, ptr_comm_target->ep_comm_ptr->message_queue, msg_block->mpi_message);
          #pragma omp flush
        }
        
        delete msg_block;
        
      }

    }

    flag = true;
    while(flag) // loop until the end of global queue "comm.mpi_comm"
    {
      Debug("Message probing for intracomm\n");
      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm> (comm.mpi_comm);
      #ifdef _openmpi
      #pragma omp critical (_mpi_call)
      {
        ::MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, mpi_comm, &flag, &status);
        if(flag)
        {
          Debug("find message in mpi comm \n");
          mpi_source = status.MPI_SOURCE;
          int tag = status.MPI_TAG;
          ::MPI_Mprobe(mpi_source, tag, mpi_comm, &message, &status);

        }
      }
      #elif _intelmpi
      ::MPI_Improbe(-2, -1, mpi_comm, &flag, &message, &status);       
      #endif

      if(flag)
      {

        MPI_Message *msg_block = new MPI_Message;
       
        msg_block->mpi_message = message;
        msg_block->ep_tag = bitset<15>(status.MPI_TAG >> 16).to_ulong();
        int src_loc       = bitset<8> (status.MPI_TAG >> 8) .to_ulong();
        int dest_loc      = bitset<8> (status.MPI_TAG)	    .to_ulong();
        int src_mpi       = status.MPI_SOURCE;
        int current_inter = comm.ep_comm_ptr->intercomm->local_rank_map->at(current_ep_rank).first;
        
        msg_block->ep_src  = get_ep_rank_intercomm(comm, src_loc, src_mpi);
        msg_block->mpi_status = new ::MPI_Status(status);
        

        MPI_Comm* ptr_comm_list = comm.ep_comm_ptr->comm_list;
        MPI_Comm* ptr_comm_target = &ptr_comm_list[dest_loc];


        #pragma omp critical (_query)
        {
          #pragma omp flush
          ptr_comm_target->ep_comm_ptr->message_queue->push_back(*msg_block);
          printf("probed one message, ep_src = %d, tag = %d, mpi_status = %p (%p), message = %d\n", msg_block->ep_src, msg_block->ep_tag, msg_block->mpi_status, &status, msg_block->mpi_message);
          #pragma omp flush
        }
        
        delete msg_block;
        
      }

    }

		return MPI_SUCCESS;
  }

}
