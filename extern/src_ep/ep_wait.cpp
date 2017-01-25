/*!
   \file ep_wait.cpp
   \since 2 may 2016

   \brief Definitions of MPI wait function: MPI_Wait, MPI_Waitall, MPI_Waitsome, MPI_Waitany
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;



namespace ep_lib {	

	int MPI_Wait(MPI_Request *request, MPI_Status *status)
	{

    if(request->type == 1)
    {
      ::MPI_Request mpi_request = static_cast< ::MPI_Request >(request->mpi_request);
      ::MPI_Status mpi_status;
      ::MPI_Wait(&mpi_request, &mpi_status);


      
      status->mpi_status = &mpi_status;
      status->ep_src = request->ep_src;
      status->ep_tag = request->ep_tag;
      status->ep_datatype = request->ep_datatype;

      return 0;
    }

    if(request->type == 2)
    {
      int flag = false;
      MPI_Message message;

      while(!flag)
      {
        Message_Check(request->comm);
        #pragma omp flush
        MPI_Improbe(request->ep_src, request->ep_tag, request->comm, &flag, &message, status);
      }

      int count;
      MPI_Get_count(status, request->ep_datatype, &count);
      MPI_Mrecv(request->buf, count, request->ep_datatype, &message, status);
      status->ep_datatype = request->ep_datatype;

      check_sum_recv(request->buf, count, request->ep_datatype, request->ep_src, request->ep_tag, request->comm, 2);

      return 0;
    }

    if(request->type == 3)
    {
      ::MPI_Request mpi_request = static_cast< ::MPI_Request >(request->mpi_request);
      ::MPI_Status mpi_status;
      ::MPI_Wait(&mpi_request, &mpi_status);
      
      status->mpi_status = new ::MPI_Status(mpi_status);
      status->ep_src = request->ep_src;
      status->ep_tag = request->ep_tag;
      status->ep_datatype = request->ep_datatype;

      int count;
      MPI_Get_count(status, request->ep_datatype, &count);
      check_sum_recv(request->buf, count, request->ep_datatype, request->ep_src, request->ep_tag, request->comm, 2);
    }
	  return MPI_SUCCESS;
	}



	


	int MPI_Waitall(int count, MPI_Request *array_of_requests, MPI_Status *array_of_statuses)
	{
    int dest_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &dest_rank);
    
    int finished = 0;
    bool finished_index[count];

	  for(int i=0; i<count; i++)
    {
      finished_index[i] = false;
    }

    while(finished < count)
    {
      for(int i=0; i<count; i++)
      {
        if(finished_index[i] == false) // this request has not been tested.
        {
          if(array_of_requests[i].type != 2) // isend or imrecv
          {      
            MPI_Wait(&array_of_requests[i], &array_of_statuses[i]);
            if(array_of_requests[i].type == 3)
            {
              int check_count;
              MPI_Get_count(&array_of_statuses[i], array_of_requests[i].ep_datatype, &check_count);
              check_sum_recv(array_of_requests[i].buf, count, array_of_requests[i].ep_datatype, array_of_requests[i].ep_src, array_of_requests[i].ep_tag, array_of_requests[i].comm, 2);
            }
            finished++;
            finished_index[i] = true;
          }
          else // irecv
          {
            int flag = false;
            MPI_Message message;
            
            MPI_Improbe(array_of_requests[i].ep_src, array_of_requests[i].ep_tag, array_of_requests[i].comm, &flag, &message, &array_of_statuses[i]);
            
            if(flag)
            {
              //printf("dest_rank = %d, Waiting one message with src = %d, tag = %d, buf = %p\n", dest_rank, array_of_requests[i].ep_src, array_of_requests[i].ep_tag, array_of_requests[i].buf);
              int recv_count;
              MPI_Get_count(&array_of_statuses[i], array_of_requests[i].ep_datatype, &recv_count);
              MPI_Mrecv(array_of_requests[i].buf, recv_count, array_of_requests[i].ep_datatype, &message, &array_of_statuses[i]);
              check_sum_recv(array_of_requests[i].buf, recv_count, array_of_requests[i].ep_datatype, array_of_requests[i].ep_src, array_of_requests[i].ep_tag, array_of_requests[i].comm, 2);

              finished++;
              finished_index[i] = true;
            }
          }
        }
      }    
    }
	  return MPI_SUCCESS;
	}





}
