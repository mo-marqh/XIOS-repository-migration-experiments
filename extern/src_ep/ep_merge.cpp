#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;


namespace ep_lib {

  int MPI_Intercomm_merge_unique_leader(MPI_Comm inter_comm, bool high, MPI_Comm *newintracomm)
  {
    Debug("intercomm_merge with unique leader\n");



    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = inter_comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = inter_comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = inter_comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = inter_comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = inter_comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = inter_comm.ep_comm_ptr->size_rank_info[2].second;

    int local_high = high;
    int remote_high;

    int remote_ep_size = inter_comm.ep_comm_ptr->intercomm->remote_rank_map->size();

    int local_ep_rank, local_ep_rank_loc, local_mpi_rank;
    int local_ep_size, local_num_ep, local_mpi_size;

    local_ep_rank = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[0].first;
    local_ep_rank_loc = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[1].first;
    local_mpi_rank = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[2].first;
    local_ep_size = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[0].second;
    local_num_ep = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[1].second;
    local_mpi_size = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[2].second;


//    if(local_ep_rank == 0 && high == false)
//    {
//      MPI_Status status;
//      MPI_Send(&local_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm);
//      MPI_Recv(&remote_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &status);
//    }
//
//    if(local_ep_rank == 0 && high == true)
//    {
//      MPI_Status status;
//      MPI_Recv(&remote_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &status);
//      MPI_Send(&local_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm);
//    }

    if(local_ep_rank == 0)
    {
      MPI_Status status;
      MPI_Request req_s, req_r;
      MPI_Isend(&local_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &req_s);
      MPI_Irecv(&remote_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &req_r);

      MPI_Wait(&req_s, &status);
      MPI_Wait(&req_r, &status);
    }


    MPI_Bcast(&remote_high, 1, MPI_INT, 0, *(inter_comm.ep_comm_ptr->intercomm->local_comm));

//    printf("%d, %d, %d, %d\n", local_ep_size, remote_ep_size, local_high, remote_high);


    MPI_Comm_dup(inter_comm, newintracomm);

    int my_ep_rank = local_high<remote_high? local_ep_rank: local_ep_rank+remote_ep_size;


    int intra_ep_rank, intra_ep_rank_loc, intra_mpi_rank;
    int intra_ep_size, intra_num_ep, intra_mpi_size;

    intra_ep_rank = newintracomm->ep_comm_ptr->size_rank_info[0].first;
    intra_ep_rank_loc = newintracomm->ep_comm_ptr->size_rank_info[1].first;
    intra_mpi_rank = newintracomm->ep_comm_ptr->size_rank_info[2].first;
    intra_ep_size = newintracomm->ep_comm_ptr->size_rank_info[0].second;
    intra_num_ep = newintracomm->ep_comm_ptr->size_rank_info[1].second;
    intra_mpi_size = newintracomm->ep_comm_ptr->size_rank_info[2].second;


    MPI_Barrier_local(*newintracomm);


    int *reorder;
    if(intra_ep_rank_loc == 0)
    {
      reorder = new int[intra_ep_size];
    }


    MPI_Gather(&my_ep_rank, 1, MPI_INT, reorder, 1, MPI_INT, 0, *newintracomm);
    if(intra_ep_rank_loc == 0)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Bcast(reorder, intra_ep_size, MPI_INT_STD, 0, static_cast< ::MPI_Comm>(newintracomm->mpi_comm));

      vector< pair<int, int> > tmp_rank_map(intra_ep_size);


      for(int i=0; i<intra_ep_size; i++)
      {
        tmp_rank_map[reorder[i]] = newintracomm->rank_map->at(i) ;
      }

      newintracomm->rank_map->swap(tmp_rank_map);

      tmp_rank_map.clear();
    }

    MPI_Barrier_local(*newintracomm);

    (*newintracomm).ep_comm_ptr->size_rank_info[0].first = my_ep_rank;

    if(intra_ep_rank_loc == 0)
    {
      delete[] reorder;
    }

    return MPI_SUCCESS;
  }





  int MPI_Intercomm_merge(MPI_Comm inter_comm, bool high, MPI_Comm *newintracomm)
  {

    assert(inter_comm.is_intercomm);

    if(inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->comm_label == -99)
    {
        return MPI_Intercomm_merge_unique_leader(inter_comm, high, newintracomm);
    }


    Debug("intercomm_merge kernel\n");

    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = inter_comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = inter_comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = inter_comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = inter_comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = inter_comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = inter_comm.ep_comm_ptr->size_rank_info[2].second;


    int local_ep_rank, local_ep_rank_loc, local_mpi_rank;
    int local_ep_size, local_num_ep, local_mpi_size;


    local_ep_rank = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[0].first;
    local_ep_rank_loc = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[1].first;
    local_mpi_rank = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[2].first;
    local_ep_size = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[0].second;
    local_num_ep = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[1].second;
    local_mpi_size = inter_comm.ep_comm_ptr->intercomm->local_comm->ep_comm_ptr->size_rank_info[2].second;

    int remote_ep_size = inter_comm.ep_comm_ptr->intercomm->remote_rank_map->size();

    int local_high = high;
    int remote_high;

    MPI_Barrier(inter_comm);

//    if(local_ep_rank == 0 && high == false)
//    {
//      MPI_Status status;
//      MPI_Send(&local_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm);
//      MPI_Recv(&remote_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &status);
//    }
//
//    if(local_ep_rank == 0 && high == true)
//    {
//      MPI_Status status;
//      MPI_Recv(&remote_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &status);
//      MPI_Send(&local_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm);
//    }

    if(local_ep_rank == 0)
    {
      MPI_Status status;
      MPI_Request req_s, req_r;
      MPI_Isend(&local_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &req_s);
      MPI_Irecv(&remote_high, 1, MPI_INT, 0, inter_comm.ep_comm_ptr->intercomm->intercomm_tag, inter_comm, &req_r);

      MPI_Wait(&req_s, &status);
      MPI_Wait(&req_r, &status);
    }

    MPI_Bcast(&remote_high, 1, MPI_INT, 0, *(inter_comm.ep_comm_ptr->intercomm->local_comm));

    int intercomm_high;
    if(ep_rank == 0) intercomm_high = local_high;
    MPI_Bcast(&intercomm_high, 1, MPI_INT, 0, inter_comm);

    //printf("remote_ep_size = %d, local_high = %d, remote_high = %d, intercomm_high = %d\n", remote_ep_size, local_high, remote_high, intercomm_high);


    ::MPI_Comm mpi_intracomm;
    MPI_Comm *ep_intracomm;

    if(ep_rank_loc == 0)
    {

      ::MPI_Comm mpi_comm = static_cast< ::MPI_Comm>(inter_comm.ep_comm_ptr->intercomm->mpi_inter_comm);
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Intercomm_merge(mpi_comm, intercomm_high, &mpi_intracomm);
      MPI_Info info;
      MPI_Comm_create_endpoints(mpi_intracomm, num_ep, info, ep_intracomm);

      inter_comm.ep_comm_ptr->comm_list->mem_bridge = ep_intracomm;

    }



    MPI_Barrier_local(inter_comm);

    *newintracomm = inter_comm.ep_comm_ptr->comm_list->mem_bridge[ep_rank_loc];

    int my_ep_rank = local_high<remote_high? local_ep_rank: local_ep_rank+remote_ep_size;

    int intra_ep_rank, intra_ep_rank_loc, intra_mpi_rank;
    int intra_ep_size, intra_num_ep, intra_mpi_size;

    intra_ep_rank = newintracomm->ep_comm_ptr->size_rank_info[0].first;
    intra_ep_rank_loc = newintracomm->ep_comm_ptr->size_rank_info[1].first;
    intra_mpi_rank = newintracomm->ep_comm_ptr->size_rank_info[2].first;
    intra_ep_size = newintracomm->ep_comm_ptr->size_rank_info[0].second;
    intra_num_ep = newintracomm->ep_comm_ptr->size_rank_info[1].second;
    intra_mpi_size = newintracomm->ep_comm_ptr->size_rank_info[2].second;



    MPI_Barrier_local(*newintracomm);


    int *reorder;
    if(intra_ep_rank_loc == 0)
    {
      reorder = new int[intra_ep_size];
    }



    MPI_Gather(&my_ep_rank, 1, MPI_INT, reorder, 1, MPI_INT, 0, *newintracomm);
    if(intra_ep_rank_loc == 0)
    {
      #ifdef _serialized
      #pragma omp critical (_mpi_call)
      #endif // _serialized
      ::MPI_Bcast(reorder, intra_ep_size, MPI_INT_STD, 0, static_cast< ::MPI_Comm>(newintracomm->mpi_comm));

      vector< pair<int, int> > tmp_rank_map(intra_ep_size);


      for(int i=0; i<intra_ep_size; i++)
      {
        tmp_rank_map[reorder[i]] = newintracomm->rank_map->at(i) ;
      }

      newintracomm->rank_map->swap(tmp_rank_map);

      tmp_rank_map.clear();
    }

    MPI_Barrier_local(*newintracomm);

    (*newintracomm).ep_comm_ptr->size_rank_info[0].first = my_ep_rank;


    if(intra_ep_rank_loc == 0)
    {
      delete[] reorder;

    }

    /*
    if(intra_ep_rank == 0)
    {
      for(int i=0; i<intra_ep_size; i++)
      {
        printf("intra rank_map[%d] = (%d, %d)\n", i, newintracomm->rank_map->at(i).first, newintracomm->rank_map->at(i).second);
      }
    }
*/
    return MPI_SUCCESS;

  }


}
