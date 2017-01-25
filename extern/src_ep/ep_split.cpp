#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;

namespace ep_lib
{

  void vec_simplify(std::vector<int> *inout_vector)
  {
    std::vector<int> out_vec;
    int found=false;
    for(std::vector<int>::iterator it_in = inout_vector->begin() ; it_in != inout_vector->end(); ++it_in)
    {
      for(std::vector<int>::iterator it = out_vec.begin() ; it != out_vec.end(); ++it)
      {
        if(*it_in == *it)
        {
          found=true;
          break;
        }
        else found=false;
      }
      if(found == false)
      {
        out_vec.push_back(*it_in);
      }
    }
    inout_vector->swap(out_vec);
  }



  int MPI_Comm_split(MPI_Comm comm, int color, int key, MPI_Comm *newcomm)
  {
    int ep_rank, ep_rank_loc, mpi_rank;
    int ep_size, num_ep, mpi_size;

    ep_rank = comm.ep_comm_ptr->size_rank_info[0].first;
    ep_rank_loc = comm.ep_comm_ptr->size_rank_info[1].first;
    mpi_rank = comm.ep_comm_ptr->size_rank_info[2].first;
    ep_size = comm.ep_comm_ptr->size_rank_info[0].second;
    num_ep = comm.ep_comm_ptr->size_rank_info[1].second;
    mpi_size = comm.ep_comm_ptr->size_rank_info[2].second;




    int num_color = 0;

    int color_index;

    vector<int> matched_number;
    vector<int> matched_number_loc;


    vector<int> all_color(ep_size);
    vector<int> all_color_loc(num_ep);

    MPI_Gather_local(&color, 1, MPI_INT, all_color_loc.data(), comm);
    MPI_Bcast_local(all_color_loc.data(), num_ep, MPI_INT, comm);


    MPI_Allgather(&color, 1, MPI_INT, all_color.data(), 1, MPI_INT, comm);

    list<int> color_list(all_color.begin(), all_color.end());
    list<int> color_list_loc(all_color_loc.begin(), all_color_loc.end());

    vec_simplify(&all_color);

    matched_number.resize(all_color.size(), 0);
    matched_number_loc.resize(all_color.size(), 0);


    while(color_list.size())
    {
      int target_color = color_list.front();
      for(list<int>::iterator it = color_list.begin(); it != color_list.end(); ++it)
      {
        if(*it == target_color)
        {
          matched_number[num_color]++;
        }
      }
      for(list<int>::iterator it = color_list_loc.begin(); it != color_list_loc.end(); ++it)
      {
        if(*it == target_color)
        {
          matched_number_loc[num_color]++;
        }
      }
      color_list.remove(target_color);
      color_list_loc.remove(target_color);
      num_color++;
    }


    vector<int> colored_key_loc[num_color];
    vector<int> key_loc(num_ep);

    MPI_Gather_local(&key, 1, MPI_INT, key_loc.data(), comm);
    MPI_Bcast_local(key_loc.data(), num_ep, MPI_INT, comm);

    for(int i=0; i<num_ep; i++)
    {
      for(int j = 0; j<num_color; j++)
      {
        if(all_color_loc[i] == all_color[j])
        {
          colored_key_loc[j].push_back(key_loc[i]);
        }
      }
    }

    int new_ep_rank_loc;

    for(int i=0; i<num_color; i++)
    {
      if(colored_key_loc[i].size()>1)
      {
        std::sort(colored_key_loc[i].begin(), colored_key_loc[i].end());
      }
      if(color == all_color[i])
      {
        color_index = i;
        for(int j=0; j<colored_key_loc[i].size(); j++)
        {
          if(key == colored_key_loc[i][j])
          {
            new_ep_rank_loc = j;
            break;
          }
        }
      }
    }

    ::MPI_Comm split_mpi_comm[num_color];

    for(int j=0; j<num_color; j++)
    {
      if(ep_rank_loc == 0)
      {
        int master_color = 1;
        if(matched_number_loc[j] == 0) master_color = MPI_UNDEFINED;
        #ifdef _serialized
        #pragma omp critical (_mpi_call)
        #endif // _serialized
        ::MPI_Comm_split(static_cast< ::MPI_Comm>(comm.mpi_comm), master_color, mpi_rank, &split_mpi_comm[j]);
        
        comm.ep_comm_ptr->comm_list->mpi_bridge = split_mpi_comm[j];
      }
      MPI_Barrier_local(comm);
      int num_new_ep = 0;

      if(new_ep_rank_loc == 0 && color == all_color[j])
      {
        num_new_ep = matched_number_loc[j];
        MPI_Info info;
        MPI_Comm *ep_comm;

        MPI_Comm_create_endpoints(comm.ep_comm_ptr->comm_list->mpi_bridge, num_new_ep, info, ep_comm);


        comm.ep_comm_ptr->comm_list->mem_bridge = ep_comm;
      }
      MPI_Barrier_local(comm);
      if(color == all_color[j])
      {
        *newcomm = comm.ep_comm_ptr->comm_list->mem_bridge[new_ep_rank_loc];
//        newcomm = &(comm.ep_comm_ptr->comm_list->mem_bridge[new_ep_rank_loc]);
        (*newcomm).ep_comm_ptr->comm_label = color;
      }
    }



    return 0;
  }

}
