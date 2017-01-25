/*!
   \file ep_test.cpp
   \since 2 may 2016

   \brief Definitions of MPI test function: MPI_Test, MPI_Testsome, MPI_Testany, MPI_Testall
 */

#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"

using namespace std;


namespace ep_lib {

	int MPI_Test(MPI_Request *request, int *flag, MPI_Status *status)
	{
		Debug("MPI_Test with EP");

    *flag = false;

    if(request->type == 1)      // isend
    {
      ::MPI_Request mpi_request = static_cast< ::MPI_Request >(request->mpi_request);
      ::MPI_Status mpi_status;
      ::MPI_Test(&mpi_request, flag, &mpi_status);
      
      if(*flag) 
      {
        status->mpi_status = new ::MPI_Status(mpi_status);
        status->ep_src = request->ep_src;
        status->ep_tag = request->ep_tag;
        status->ep_datatype = request->ep_datatype;
      }

      return 0;
    }

    if(request->type == 2)   // irecv message not probed
    {
      Message_Check(request->comm);
      #pragma omp flush
      MPI_Message message;
      MPI_Improbe(request->ep_src, request->ep_tag, request->comm, flag, &message, status);
      if(*flag)
      {
        int count;
        MPI_Get_count(status, request->ep_datatype, &count);
        MPI_Imrecv(request->buf, count, request->ep_datatype, &message, request);
        MPI_Test(request, flag, status);
      }
      return 0;
    }

    if(request->type == 3)  // imrecv
    {
      ::MPI_Request mpi_request = static_cast< ::MPI_Request >(request->mpi_request);
      ::MPI_Status mpi_status;
      ::MPI_Test(&mpi_request, flag, &mpi_status);
      if(*flag)
      {
        status->mpi_status = new ::MPI_Status(mpi_status);
        status->ep_src = request->ep_src;
      status->ep_tag = request->ep_tag;
      status->ep_datatype = request->ep_datatype;
        int count;
        MPI_Get_count(status, request->ep_datatype, &count);
        check_sum_recv(request->buf, count, request->ep_datatype, request->ep_src, request->ep_tag, request->comm, 2);
      }

      status->ep_src = request->ep_src;
      status->ep_tag = request->ep_tag;
      status->ep_datatype = request->ep_datatype;

      

      return 0;
    }
		
	}


	int MPI_Testall(int count, MPI_Request *array_of_requests, int *flag, MPI_Status *array_of_statuses)
	{
	  Debug("MPI_Testall with EP");
    *flag = true;
    int i=0;
    while(*flag && i<count )
    {
      MPI_Test(&array_of_requests[i], flag, &array_of_statuses[i]);
      i++;
    }
	}


}
