#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;

namespace ep_lib
{

  #ifdef _openmpi

  int MPI_Intercomm_create_from_world(MPI_Comm local_comm, int local_leader, void* peer_comm_ptr, int mpi_remote_leader, int tag, MPI_Comm *newintercomm)
  {

    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = local_comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = local_comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = local_comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = local_comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = local_comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = local_comm.ep_comm_ptr->size_rank_info[2].second;


    std::vector<int> rank_info[4];  //! 0->rank_in_world of local_comm,  1->rank_in_local_parent of local_comm
                                    //! 2->rank_in_world of remote_comm, 3->rank_in_local_parent of remote_comm

    int rank_in_world;
    int rank_in_local_parent;

    int local_ep_size = ep_size;
    int remote_ep_size;

    ::MPI_Comm peer_comm = static_cast< ::MPI_Comm>(peer_comm_ptr);
    ::MPI_Comm local_mpi_comm = static_cast< ::MPI_Comm>(local_comm.mpi_comm);

    ::MPI_Comm_rank(peer_comm, &rank_in_world);

    ::MPI_Comm_rank(static_cast< ::MPI_Comm>(local_comm.mpi_comm), &rank_in_local_parent);

    bool is_proc_master = false;
    bool is_local_leader = false;
    bool is_final_master = false;


    if(ep_rank == local_leader) { is_proc_master = true; is_local_leader = true; is_final_master = true;}
    if(ep_rank_loc == 0 && mpi_rank != local_comm.rank_map->at(local_leader).second) is_proc_master = true;


    int size_info[4]; //! used for choose size of rank_info 0-> mpi_size of local_comm, 1-> mpi_size of remote_comm

    int leader_info[4]; //! 0->world rank of local_leader, 1->world rank of remote leader


    std::vector<int> ep_info[2]; //! 0-> num_ep in local_comm, 1->num_ep in remote_comm

    std::vector<int> new_rank_info[4];
    std::vector<int> new_ep_info[2];

    std::vector<int> offset;

    if(is_proc_master)
    {

      size_info[0] = mpi_size;

      rank_info[0].resize(size_info[0]);
      rank_info[1].resize(size_info[0]);



      ep_info[0].resize(size_info[0]);

      vector<int> send_buf(6);
      vector<int> recv_buf(3*size_info[0]);

      send_buf[0] = rank_in_world;
      send_buf[1] = rank_in_local_parent;
      send_buf[2] = num_ep;

      ::MPI_Allgather(send_buf.data(), 3, MPI_INT_STD, recv_buf.data(), 3, MPI_INT_STD, local_mpi_comm);

      for(int i=0; i<size_info[0]; i++)
      {
        rank_info[0][i] = recv_buf[3*i];
        rank_info[1][i] = recv_buf[3*i+1];
        ep_info[0][i]   = recv_buf[3*i+2];
      }

      if(is_local_leader)
      {
        leader_info[0] = rank_in_world;
        leader_info[1] = mpi_remote_leader;

        ::MPI_Status mpi_status;

        send_buf[0] = size_info[0];
        send_buf[1] = local_ep_size;

        ::MPI_Send(send_buf.data(), 2, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(recv_buf.data(), 2, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);

        recv_buf[2] = rank_in_world;
        recv_buf[3] = mpi_remote_leader;

      }

      ::MPI_Bcast(recv_buf.data(), 4, MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      size_info[1] = recv_buf[0];
      remote_ep_size = recv_buf[1];
      leader_info[0] = recv_buf[2];
      leader_info[1] = recv_buf[3];

      rank_info[2].resize(size_info[1]);
      rank_info[3].resize(size_info[1]);

      ep_info[1].resize(size_info[1]);

      send_buf.resize(3*size_info[0]);
      recv_buf.resize(3*size_info[1]);

      if(is_local_leader)
      {
        ::MPI_Status mpi_status;


        std::copy ( rank_info[0].data(), rank_info[0].data() + size_info[0], send_buf.begin() );
        std::copy ( rank_info[1].data(), rank_info[1].data() + size_info[0], send_buf.begin() + size_info[0] );
        std::copy ( ep_info[0].data(),   ep_info[0].data()   + size_info[0], send_buf.begin() + 2*size_info[0] );

        ::MPI_Send(send_buf.data(), 3*size_info[0], MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(recv_buf.data(), 3*size_info[1], MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);

      }

      ::MPI_Bcast(recv_buf.data(), 3*size_info[1], MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      std::copy ( recv_buf.data(), recv_buf.data() + size_info[1], rank_info[2].begin() );
      std::copy ( recv_buf.data() + size_info[1], recv_buf.data() + 2*size_info[1], rank_info[3].begin()  );
      std::copy ( recv_buf.data() + 2*size_info[1], recv_buf.data() + 3*size_info[1], ep_info[1].begin() );

      offset.resize(size_info[0]);

      if(leader_info[0]<leader_info[1]) // erase all ranks doubled with remote_comm, except the local leader
      {

        bool found = false;
        int ep_tmp;
        int ep_local;
        int ep_remote;
        for(int i=0; i<size_info[0]; i++)
        {
          int target = rank_info[0][i];
          found = false;
          for(int j=0; j<size_info[1]; j++)
          {
            if(target == rank_info[2][j])
            {
              found = true;
              ep_tmp = ep_info[1][j];
              ep_local = ep_info[0][j];
              ep_remote = ep_info[1][j];
              break;
            }
          }
          if(found)
          {

            if(target == leader_info[0]) // the leader is doubled in remote
            {
              new_rank_info[0].push_back(target);
              new_rank_info[1].push_back(rank_info[1][i]);

              new_ep_info[0].push_back(ep_local + ep_remote);
              offset[i] = 0;
            }
            else
            {
              offset[i] = ep_local;
            }
          }
          else
          {
            new_rank_info[0].push_back(target);
            new_rank_info[1].push_back(rank_info[1][i]);

            new_ep_info[0].push_back(ep_info[0][i]);

            offset[i] = 0;
          }

        }
      }

      else // erase rank doubled with remote leader
      {

        bool found = false;
        int ep_tmp;
        int ep_local;
        int ep_remote;
        for(int i=0; i<size_info[0]; i++)
        {
          int target = rank_info[0][i];
          found = false;
          for(int j=0; j<size_info[1]; j++)
          {

            if(target == rank_info[2][j])
            {
              found = true;
              ep_tmp = ep_info[1][j];
              ep_local = ep_info[0][j];
              ep_remote = ep_info[1][j];
              break;
            }
          }
          if(found)
          {
            if(target != leader_info[1])
            {
              new_rank_info[0].push_back(target);
              new_rank_info[1].push_back(rank_info[1][i]);

              new_ep_info[0].push_back(ep_local + ep_remote);
              offset[i] = 0;
            }
            else // found remote leader
            {
              offset[i] = ep_remote;
            }
          }
          else
          {
            new_rank_info[0].push_back(target);
            new_rank_info[1].push_back(rank_info[1][i]);

            new_ep_info[0].push_back(ep_info[0][i]);
            offset[i] = 0;
          }
        }
      }

      if(offset[mpi_rank] == 0)
      {
        is_final_master = true;
      }


      // size_info[4]: 2->size of new_ep_info for local, 3->size of new_ep_info for remote

      if(is_local_leader)
      {
        size_info[2] = new_ep_info[0].size();
        ::MPI_Status mpi_status;

        ::MPI_Send(&size_info[2], 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(&size_info[3], 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);
      }

      ::MPI_Bcast(&size_info[2], 2, MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      new_rank_info[2].resize(size_info[3]);
      new_rank_info[3].resize(size_info[3]);
      new_ep_info[1].resize(size_info[3]);

      send_buf.resize(size_info[2]);
      recv_buf.resize(size_info[3]);

      if(is_local_leader)
      {
        ::MPI_Status mpi_status;

        std::copy ( new_rank_info[0].data(), new_rank_info[0].data() + size_info[2], send_buf.begin() );
        std::copy ( new_rank_info[1].data(), new_rank_info[1].data() + size_info[2], send_buf.begin() + size_info[2] );
        std::copy ( new_ep_info[0].data(),   new_ep_info[0].data()   + size_info[0], send_buf.begin() + 2*size_info[2] );

        MPI_Send(send_buf.data(), 3*size_info[2], MPI_INT_STD, mpi_remote_leader, tag, peer_comm);
        MPI_Recv(recv_buf.data(), 3*size_info[3], MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);

      }

      ::MPI_Bcast(recv_buf.data(),   3*size_info[3], MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      std::copy ( recv_buf.data(), recv_buf.data() + size_info[3], new_rank_info[2].begin() );
      std::copy ( recv_buf.data() + size_info[3], recv_buf.data() + 2*size_info[3], new_rank_info[3].begin()  );
      std::copy ( recv_buf.data() + 2*size_info[3], recv_buf.data() + 3*size_info[3], new_ep_info[1].begin() );

    }



    if(is_proc_master)
    {
      // leader_info[4]: 2-> rank of local leader in new_group generated comm;
                      // 3-> rank of remote leader in new_group generated comm;
      ::MPI_Group local_group;
      ::MPI_Group new_group;
      ::MPI_Comm new_comm;
      ::MPI_Comm intercomm;

      ::MPI_Comm_group(local_mpi_comm, &local_group);

      ::MPI_Group_incl(local_group, size_info[2], new_rank_info[1].data(), &new_group);

      ::MPI_Comm_create(local_mpi_comm, new_group, &new_comm);



      if(is_local_leader)
      {
        ::MPI_Comm_rank(new_comm, &leader_info[2]);
      }

      ::MPI_Bcast(&leader_info[2], 1, MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      if(new_comm != MPI_COMM_NULL_STD)
      {
        ::MPI_Barrier(new_comm);

        ::MPI_Intercomm_create(new_comm, leader_info[2], peer_comm, leader_info[1], tag, &intercomm);

        int id;
        MPI_Comm_rank(new_comm, &id);
        int my_num_ep = new_ep_info[0][id];

        MPI_Comm *ep_intercomm;
        MPI_Info info;
        MPI_Comm_create_endpoints(new_comm, my_num_ep, info, ep_intercomm);



        for(int i= 0; i<my_num_ep; i++)
        {
          ep_intercomm[i].is_intercomm = true;

          ep_intercomm[i].ep_comm_ptr->intercomm = new ep_lib::ep_intercomm;
          ep_intercomm[i].ep_comm_ptr->intercomm->mpi_inter_comm = intercomm;
          ep_intercomm[i].ep_comm_ptr->comm_label = leader_info[0];
        }

        #pragma omp critical (write_to_tag_list)
        tag_list.push_back(make_pair( make_pair(tag, min(leader_info[0], leader_info[1])) , ep_intercomm));
      }


    }


    MPI_Barrier_local(local_comm);

    vector<int> bcast_buf(8);
    if(is_local_leader)
    {
      std::copy(size_info, size_info+4, bcast_buf.begin());
      std::copy(leader_info, leader_info+4, bcast_buf.begin()+4);
    }

    MPI_Bcast(bcast_buf.data(), 8, MPI_INT_STD, local_leader, local_comm);

    if(!is_local_leader)
    {
      std::copy(bcast_buf.begin(), bcast_buf.begin()+4, size_info);
      std::copy(bcast_buf.begin()+4, bcast_buf.begin()+8, leader_info);
    }

    if(!is_local_leader)
    {
      new_rank_info[1].resize(size_info[2]);
      ep_info[1].resize(size_info[1]);
      offset.resize(size_info[0]);
    }

    bcast_buf.resize(size_info[2]+size_info[1]+size_info[0]+1);

    if(is_local_leader)
    {
      bcast_buf[0] = remote_ep_size;
      std::copy(new_rank_info[1].data(), new_rank_info[1].data()+size_info[2], bcast_buf.begin()+1);
      std::copy(ep_info[1].data(), ep_info[1].data()+size_info[1], bcast_buf.begin()+size_info[2]+1);
      std::copy(offset.data(), offset.data()+size_info[0], bcast_buf.begin()+size_info[2]+size_info[1]+1);
    }

    MPI_Bcast(bcast_buf.data(), size_info[2]+size_info[1]+size_info[0]+1, MPI_INT_STD, local_leader, local_comm);

    if(!is_local_leader)
    {
      remote_ep_size = bcast_buf[0];
      std::copy(bcast_buf.data()+1, bcast_buf.data()+1+size_info[2], new_rank_info[1].begin());
      std::copy(bcast_buf.data()+1+size_info[2], bcast_buf.data()+1+size_info[2]+size_info[1], ep_info[1].begin());
      std::copy(bcast_buf.data()+1+size_info[2]+size_info[1], bcast_buf.data()+1+size_info[2]+size_info[1]+size_info[0], offset.begin());
    }


    int my_position = offset[rank_in_local_parent]+ep_rank_loc;


    MPI_Barrier_local(local_comm);
    #pragma omp flush


    #pragma omp critical (read_from_tag_list)
    {
      bool found = false;
      while(!found)
      {
        for(std::list<std::pair < std::pair<int,int>, MPI_Comm* > >::iterator iter = tag_list.begin(); iter!=tag_list.end(); iter++)
        {
          if((*iter).first == make_pair(tag, min(leader_info[0], leader_info[1])))
          {
            *newintercomm =  iter->second[my_position];

            found = true;
            break;
          }
        }
      }
    }

    MPI_Barrier_local(local_comm);

    int intercomm_ep_rank, intercomm_ep_rank_loc, intercomm_mpi_rank;
    int intercomm_ep_size, intercomm_num_ep, intercomm_mpi_size;

    intercomm_ep_rank = newintercomm->ep_comm_ptr->size_rank_info[0].first;
    intercomm_ep_rank_loc = newintercomm->ep_comm_ptr->size_rank_info[1].first;
    intercomm_mpi_rank = newintercomm->ep_comm_ptr->size_rank_info[2].first;
    intercomm_ep_size = newintercomm->ep_comm_ptr->size_rank_info[0].second;
    intercomm_num_ep = newintercomm->ep_comm_ptr->size_rank_info[1].second;
    intercomm_mpi_size = newintercomm->ep_comm_ptr->size_rank_info[2].second;

    MPI_Bcast(&remote_ep_size, 1, MPI_INT_STD, local_leader, local_comm);

    int my_rank_map_elem[2];


    my_rank_map_elem[0] = intercomm_ep_rank;

    my_rank_map_elem[1] = (*newintercomm).ep_comm_ptr->comm_label;

    vector<pair<int, int> > local_rank_map_array;
    vector<pair<int, int> > remote_rank_map_array;


    (*newintercomm).ep_comm_ptr->intercomm->local_rank_map = new RANK_MAP;
    (*newintercomm).ep_comm_ptr->intercomm->local_rank_map->resize(local_ep_size);

    MPI_Allgather(my_rank_map_elem, 2, MPI_INT_STD, (*newintercomm).ep_comm_ptr->intercomm->local_rank_map->data(), 2, MPI_INT_STD, local_comm);

    (*newintercomm).ep_comm_ptr->intercomm->remote_rank_map = new RANK_MAP;
    (*newintercomm).ep_comm_ptr->intercomm->remote_rank_map->resize(remote_ep_size);

    int local_intercomm_size = intercomm_ep_size;
    int remote_intercomm_size;




    if(is_local_leader)
    {
      ::MPI_Status status;

      ::MPI_Send((*newintercomm).ep_comm_ptr->intercomm->local_rank_map->data(), 2*local_ep_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

      ::MPI_Recv((*newintercomm).ep_comm_ptr->intercomm->remote_rank_map->data(), 2*remote_ep_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &status);

      ::MPI_Send(&local_intercomm_size, 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

      ::MPI_Recv(&remote_intercomm_size, 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &status);
    }

    MPI_Bcast((*newintercomm).ep_comm_ptr->intercomm->remote_rank_map->data(), 2*remote_ep_size, MPI_INT_STD, local_leader, local_comm);
    MPI_Bcast(&remote_intercomm_size, 1, MPI_INT_STD, 0, *newintercomm);


    (*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map = new RANK_MAP;
    (*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->resize(remote_intercomm_size);

    (*newintercomm).ep_comm_ptr->intercomm->size_rank_info[0] = local_comm.ep_comm_ptr->size_rank_info[0];
    (*newintercomm).ep_comm_ptr->intercomm->size_rank_info[1] = local_comm.ep_comm_ptr->size_rank_info[1];
    (*newintercomm).ep_comm_ptr->intercomm->size_rank_info[2] = local_comm.ep_comm_ptr->size_rank_info[2];


    if(is_local_leader)
    {
      ::MPI_Status status;

      ::MPI_Send((*newintercomm).rank_map->data(), 2*local_intercomm_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

      ::MPI_Recv((*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->data(), 2*remote_intercomm_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &status);
    }

    MPI_Bcast((*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->data(), 2*remote_intercomm_size, MPI_INT_STD, 0, *newintercomm);

    (*newintercomm).ep_comm_ptr->intercomm->local_comm = &(local_comm.ep_comm_ptr->comm_list[ep_rank_loc]);
    (*newintercomm).ep_comm_ptr->intercomm->intercomm_tag = local_comm.ep_comm_ptr->comm_label;


    return MPI_SUCCESS;

  }



  #elif _intelmpi
  int MPI_Intercomm_create_from_world(MPI_Comm local_comm, int local_leader, int peer_comm_ptr, int mpi_remote_leader, int tag, MPI_Comm *newintercomm)
  {
    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = local_comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = local_comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = local_comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = local_comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = local_comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = local_comm.ep_comm_ptr->size_rank_info[2].second;

    std::vector<int> rank_info[4];  //! 0->rank_in_world of local_comm,  1->rank_in_local_parent of local_comm
                                    //! 2->rank_in_world of remote_comm, 3->rank_in_local_parent of remote_comm

    int rank_in_world;
    int rank_in_local_parent;

    int local_ep_size = ep_size;
    int remote_ep_size;

    ::MPI_Comm peer_comm = static_cast< ::MPI_Comm>(peer_comm_ptr);
    ::MPI_Comm local_mpi_comm = static_cast< ::MPI_Comm>(local_comm.mpi_comm);

    ::MPI_Comm_rank(peer_comm, &rank_in_world);

    ::MPI_Comm_rank(static_cast< ::MPI_Comm>(local_comm.mpi_comm), &rank_in_local_parent);

    bool is_proc_master = false;
    bool is_local_leader = false;
    bool is_final_master = false;


    if(ep_rank == local_leader) { is_proc_master = true; is_local_leader = true; is_final_master = true;}
    if(ep_rank_loc == 0 && mpi_rank != local_comm.rank_map->at(local_leader).second) is_proc_master = true;


    int size_info[4]; //! used for choose size of rank_info 0-> mpi_size of local_comm, 1-> mpi_size of remote_comm

    int leader_info[4]; //! 0->world rank of local_leader, 1->world rank of remote leader


    std::vector<int> ep_info[2]; //! 0-> num_ep in local_comm, 1->num_ep in remote_comm

    std::vector<int> new_rank_info[4];
    std::vector<int> new_ep_info[2];

    std::vector<int> offset;

    if(is_proc_master)
    {

      size_info[0] = mpi_size;

      rank_info[0].resize(size_info[0]);
      rank_info[1].resize(size_info[0]);

      ep_info[0].resize(size_info[0]);

      vector<int> send_buf(6);
      vector<int> recv_buf(3*size_info[0]);

      send_buf[0] = rank_in_world;
      send_buf[1] = rank_in_local_parent;
      send_buf[2] = num_ep;

      ::MPI_Allgather(send_buf.data(), 3, MPI_INT_STD, recv_buf.data(), 3, MPI_INT_STD, local_mpi_comm);

      for(int i=0; i<size_info[0]; i++)
      {
        rank_info[0][i] = recv_buf[3*i];
        rank_info[1][i] = recv_buf[3*i+1];
        ep_info[0][i]   = recv_buf[3*i+2];
      }


      if(is_local_leader)
      {
        leader_info[0] = rank_in_world;
        leader_info[1] = mpi_remote_leader;

        ::MPI_Status mpi_status;

        send_buf[0] = size_info[0];
        send_buf[1] = local_ep_size;

        ::MPI_Send(send_buf.data(), 2, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(recv_buf.data(), 2, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);

        recv_buf[2] = rank_in_world;
        recv_buf[3] = mpi_remote_leader;

      }

      ::MPI_Bcast(recv_buf.data(), 4, MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      size_info[1] = recv_buf[0];
      remote_ep_size = recv_buf[1];
      leader_info[0] = recv_buf[2];
      leader_info[1] = recv_buf[3];

      rank_info[2].resize(size_info[1]);
      rank_info[3].resize(size_info[1]);

      ep_info[1].resize(size_info[1]);

      send_buf.resize(3*size_info[0]);
      recv_buf.resize(3*size_info[1]);

      if(is_local_leader)
      {
        ::MPI_Status mpi_status;


        std::copy ( rank_info[0].data(), rank_info[0].data() + size_info[0], send_buf.begin() );
        std::copy ( rank_info[1].data(), rank_info[1].data() + size_info[0], send_buf.begin() + size_info[0] );
        std::copy ( ep_info[0].data(),   ep_info[0].data()   + size_info[0], send_buf.begin() + 2*size_info[0] );

        ::MPI_Send(send_buf.data(), 3*size_info[0], MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(recv_buf.data(), 3*size_info[1], MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);

      }

      ::MPI_Bcast(recv_buf.data(), 3*size_info[1], MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      std::copy ( recv_buf.data(), recv_buf.data() + size_info[1], rank_info[2].begin() );
      std::copy ( recv_buf.data() + size_info[1], recv_buf.data() + 2*size_info[1], rank_info[3].begin()  );
      std::copy ( recv_buf.data() + 2*size_info[1], recv_buf.data() + 3*size_info[1], ep_info[1].begin() );

      offset.resize(size_info[0]);

      if(leader_info[0]<leader_info[1]) // erase all ranks doubled with remote_comm, except the local leader
      {

        bool found = false;
        int ep_tmp;
        int ep_local;
        int ep_remote;
        for(int i=0; i<size_info[0]; i++)
        {
          int target = rank_info[0][i];
          found = false;
          for(int j=0; j<size_info[1]; j++)
          {
            if(target == rank_info[2][j])
            {
              found = true;
              ep_tmp = ep_info[1][j];
              ep_local = ep_info[0][j];
              ep_remote = ep_info[1][j];
              break;
            }
          }
          if(found)
          {

            if(target == leader_info[0]) // the leader is doubled in remote
            {
              new_rank_info[0].push_back(target);
              new_rank_info[1].push_back(rank_info[1][i]);

              new_ep_info[0].push_back(ep_local + ep_remote);
              offset[i] = 0;
            }
            else
            {
              offset[i] = ep_local;
            }
          }
          else
          {
            new_rank_info[0].push_back(target);
            new_rank_info[1].push_back(rank_info[1][i]);

            new_ep_info[0].push_back(ep_info[0][i]);

            offset[i] = 0;
          }

        }
      }

      else // erase rank doubled with remote leader
      {

        bool found = false;
        int ep_tmp;
        int ep_local;
        int ep_remote;
        for(int i=0; i<size_info[0]; i++)
        {
          int target = rank_info[0][i];
          found = false;
          for(int j=0; j<size_info[1]; j++)
          {

            if(target == rank_info[2][j])
            {
              found = true;
              ep_tmp = ep_info[1][j];
              ep_local = ep_info[0][j];
              ep_remote = ep_info[1][j];
              break;
            }
          }
          if(found)
          {
            if(target != leader_info[1])
            {
              new_rank_info[0].push_back(target);
              new_rank_info[1].push_back(rank_info[1][i]);

              new_ep_info[0].push_back(ep_local + ep_remote);
              offset[i] = 0;
            }
            else // found remote leader
            {
              offset[i] = ep_remote;
            }
          }
          else
          {
            new_rank_info[0].push_back(target);
            new_rank_info[1].push_back(rank_info[1][i]);

            new_ep_info[0].push_back(ep_info[0][i]);
            offset[i] = 0;
          }
        }
      }

      if(offset[mpi_rank] == 0)
      {
        is_final_master = true;
      }


      // size_info[4]: 2->size of new_ep_info for local, 3->size of new_ep_info for remote

      if(is_local_leader)
      {
        size_info[2] = new_ep_info[0].size();
        ::MPI_Status mpi_status;

        ::MPI_Send(&size_info[2], 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(&size_info[3], 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);
      }

      ::MPI_Bcast(&size_info[2], 2, MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      new_rank_info[2].resize(size_info[3]);
      new_rank_info[3].resize(size_info[3]);
      new_ep_info[1].resize(size_info[3]);

      send_buf.resize(size_info[2]);
      recv_buf.resize(size_info[3]);

      if(is_local_leader)
      {
        ::MPI_Status mpi_status;

        std::copy ( new_rank_info[0].data(), new_rank_info[0].data() + size_info[2], send_buf.begin() );
        std::copy ( new_rank_info[1].data(), new_rank_info[1].data() + size_info[2], send_buf.begin() + size_info[2] );
        std::copy ( new_ep_info[0].data(),   new_ep_info[0].data()   + size_info[0], send_buf.begin() + 2*size_info[2] );

        ::MPI_Send(send_buf.data(), 3*size_info[2], MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

        ::MPI_Recv(recv_buf.data(), 3*size_info[3], MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &mpi_status);
      }

      ::MPI_Bcast(recv_buf.data(),   3*size_info[3], MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      std::copy ( recv_buf.data(), recv_buf.data() + size_info[3], new_rank_info[2].begin() );
      std::copy ( recv_buf.data() + size_info[3], recv_buf.data() + 2*size_info[3], new_rank_info[3].begin()  );
      std::copy ( recv_buf.data() + 2*size_info[3], recv_buf.data() + 3*size_info[3], new_ep_info[1].begin() );

    }



    if(is_proc_master)
    {
      // leader_info[4]: 2-> rank of local leader in new_group generated comm;
                      // 3-> rank of remote leader in new_group generated comm;
      ::MPI_Group local_group;
      ::MPI_Group new_group;
      ::MPI_Comm new_comm;
      ::MPI_Comm intercomm;

      ::MPI_Comm_group(local_mpi_comm, &local_group);

      ::MPI_Group_incl(local_group, size_info[2], new_rank_info[1].data(), &new_group);

      ::MPI_Comm_create(local_mpi_comm, new_group, &new_comm);



      if(is_local_leader)
      {
        ::MPI_Comm_rank(new_comm, &leader_info[2]);
      }

      ::MPI_Bcast(&leader_info[2], 1, MPI_INT_STD, local_comm.rank_map->at(local_leader).second, local_mpi_comm);

      if(new_comm != MPI_COMM_NULL_STD)
      {
        ::MPI_Barrier(new_comm);

        ::MPI_Intercomm_create(new_comm, leader_info[2], peer_comm, leader_info[1], tag, &intercomm);

        int id;
        ::MPI_Comm_rank(new_comm, &id);
        int my_num_ep = new_ep_info[0][id];

        MPI_Comm *ep_intercomm;
        MPI_Info info;
        MPI_Comm_create_endpoints(new_comm, my_num_ep, info, ep_intercomm);

        for(int i= 0; i<my_num_ep; i++)
        {
          ep_intercomm[i].is_intercomm = true;

          ep_intercomm[i].ep_comm_ptr->intercomm = new ep_lib::ep_intercomm;
          ep_intercomm[i].ep_comm_ptr->intercomm->mpi_inter_comm = intercomm;
          ep_intercomm[i].ep_comm_ptr->comm_label = leader_info[0];
        }


        #pragma omp critical (write_to_tag_list)
        tag_list.push_back(make_pair( make_pair(tag, min(leader_info[0], leader_info[1])) , ep_intercomm));

      }


    }


    MPI_Barrier_local(local_comm);


    vector<int> bcast_buf(8);
    if(is_local_leader)
    {
      std::copy(size_info, size_info+4, bcast_buf.begin());
      std::copy(leader_info, leader_info+4, bcast_buf.begin()+4);
    }



    MPI_Bcast(bcast_buf.data(), 8, MPI_INT_STD, local_leader, local_comm);


    if(!is_local_leader)
    {
      std::copy(bcast_buf.begin(), bcast_buf.begin()+4, size_info);
      std::copy(bcast_buf.begin()+4, bcast_buf.begin()+8, leader_info);
    }



    if(!is_local_leader)
    {
      new_rank_info[1].resize(size_info[2]);
      ep_info[1].resize(size_info[1]);
      offset.resize(size_info[0]);
    }

    bcast_buf.resize(size_info[2]+size_info[1]+size_info[0]+1);

    if(is_local_leader)
    {
      bcast_buf[0] = remote_ep_size;
      std::copy(new_rank_info[1].data(), new_rank_info[1].data()+size_info[2], bcast_buf.begin()+1);
      std::copy(ep_info[1].data(), ep_info[1].data()+size_info[1], bcast_buf.begin()+size_info[2]+1);
      std::copy(offset.data(), offset.data()+size_info[0], bcast_buf.begin()+size_info[2]+size_info[1]+1);
    }

    MPI_Bcast(bcast_buf.data(), size_info[2]+size_info[1]+size_info[0]+1, MPI_INT_STD, local_leader, local_comm);


    if(!is_local_leader)
    {
      remote_ep_size = bcast_buf[0];
      std::copy(bcast_buf.data()+1, bcast_buf.data()+1+size_info[2], new_rank_info[1].begin());
      std::copy(bcast_buf.data()+1+size_info[2], bcast_buf.data()+1+size_info[2]+size_info[1], ep_info[1].begin());
      std::copy(bcast_buf.data()+1+size_info[2]+size_info[1], bcast_buf.data()+1+size_info[2]+size_info[1]+size_info[0], offset.begin());
    }


    int my_position = offset[rank_in_local_parent]+ep_rank_loc;


    MPI_Barrier_local(local_comm);
    #pragma omp flush


    #pragma omp critical (read_from_tag_list)
    {
      bool found = false;
      while(!found)
      {
        for(std::list<std::pair < std::pair<int,int>, MPI_Comm* > >::iterator iter = tag_list.begin(); iter!=tag_list.end(); iter++)
        {
          if((*iter).first == make_pair(tag, min(leader_info[0], leader_info[1])))
          {
            *newintercomm =  iter->second[my_position];

            found = true;
            break;
          }
        }
      }
    }

    MPI_Barrier_local(local_comm);

    int intercomm_ep_rank, intercomm_ep_rank_loc, intercomm_mpi_rank;
    int intercomm_ep_size, intercomm_num_ep, intercomm_mpi_size;

    intercomm_ep_rank = newintercomm->ep_comm_ptr->size_rank_info[0].first;
    intercomm_ep_rank_loc = newintercomm->ep_comm_ptr->size_rank_info[1].first;
    intercomm_mpi_rank = newintercomm->ep_comm_ptr->size_rank_info[2].first;
    intercomm_ep_size = newintercomm->ep_comm_ptr->size_rank_info[0].second;
    intercomm_num_ep = newintercomm->ep_comm_ptr->size_rank_info[1].second;
    intercomm_mpi_size = newintercomm->ep_comm_ptr->size_rank_info[2].second;


    MPI_Bcast(&remote_ep_size, 1, MPI_INT_STD, local_leader, local_comm);

    int my_rank_map_elem[2];


    my_rank_map_elem[0] = intercomm_ep_rank;

    my_rank_map_elem[1] = (*newintercomm).ep_comm_ptr->comm_label;

    vector<pair<int, int> > local_rank_map_array;
    vector<pair<int, int> > remote_rank_map_array;


    (*newintercomm).ep_comm_ptr->intercomm->local_rank_map = new RANK_MAP;
    (*newintercomm).ep_comm_ptr->intercomm->local_rank_map->resize(local_ep_size);

    MPI_Allgather(my_rank_map_elem, 2, MPI_INT_STD, (*newintercomm).ep_comm_ptr->intercomm->local_rank_map->data(), 2, MPI_INT_STD, local_comm);

    (*newintercomm).ep_comm_ptr->intercomm->remote_rank_map = new RANK_MAP;
    (*newintercomm).ep_comm_ptr->intercomm->remote_rank_map->resize(remote_ep_size);

    int local_intercomm_size = intercomm_ep_size;
    int remote_intercomm_size;


    if(is_local_leader)
    {
      ::MPI_Status status;

      ::MPI_Send((*newintercomm).ep_comm_ptr->intercomm->local_rank_map->data(), 2*local_ep_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

      ::MPI_Recv((*newintercomm).ep_comm_ptr->intercomm->remote_rank_map->data(), 2*remote_ep_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &status);

      ::MPI_Send(&local_intercomm_size, 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

      ::MPI_Recv(&remote_intercomm_size, 1, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &status);
    }

    MPI_Bcast((*newintercomm).ep_comm_ptr->intercomm->remote_rank_map->data(), 2*remote_ep_size, MPI_INT_STD, local_leader, local_comm);
    MPI_Bcast(&remote_intercomm_size, 1, MPI_INT_STD, 0, *newintercomm);


    (*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map = new RANK_MAP;
    (*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->resize(remote_intercomm_size);

    (*newintercomm).ep_comm_ptr->intercomm->size_rank_info[0] = local_comm.ep_comm_ptr->size_rank_info[0];
    (*newintercomm).ep_comm_ptr->intercomm->size_rank_info[1] = local_comm.ep_comm_ptr->size_rank_info[1];
    (*newintercomm).ep_comm_ptr->intercomm->size_rank_info[2] = local_comm.ep_comm_ptr->size_rank_info[2];


    if(is_local_leader)
    {
      ::MPI_Status status;

      ::MPI_Send((*newintercomm).rank_map->data(), 2*local_intercomm_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm);

      ::MPI_Recv((*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->data(), 2*remote_intercomm_size, MPI_INT_STD, mpi_remote_leader, tag, peer_comm, &status);
    }

    MPI_Bcast((*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->data(), 2*remote_intercomm_size, MPI_INT_STD, 0, *newintercomm);

    (*newintercomm).ep_comm_ptr->intercomm->local_comm = &(local_comm.ep_comm_ptr->comm_list[ep_rank_loc]);
    (*newintercomm).ep_comm_ptr->intercomm->intercomm_tag = local_comm.ep_comm_ptr->comm_label;


//    for(int i=0; i<local_ep_size; i++)
//    if(local_comm.ep_comm_ptr->comm_label != 99) printf("ep_rank = %d, remote_rank_map[%d] = (%d,%d)\n", intercomm_ep_rank, i,
//          (*newintercomm).ep_comm_ptr->intercomm->local_rank_map->at(i).first, (*newintercomm).ep_comm_ptr->intercomm->local_rank_map->at(i).second);

//    for(int i=0; i<remote_intercomm_size; i++)
//    if(local_comm.ep_comm_ptr->comm_label != 99) printf("ep_rank = %d, intercomm_rank_map[%d] = (%d,%d)\n", intercomm_ep_rank, i,
//          (*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->at(i).first, (*newintercomm).ep_comm_ptr->intercomm->intercomm_rank_map->at(i).second);

//    for(int i=0; i<(*newintercomm).rank_map->size(); i++)
//    if(local_comm.ep_comm_ptr->comm_label != 99) printf("ep_rank = %d, rank_map[%d] = (%d,%d)\n", intercomm_ep_rank, i,
//          (*newintercomm).rank_map->at(i).first, (*newintercomm).rank_map->at(i).second);





    return MPI_SUCCESS;

  }

  #endif

}

