#ifndef EP_TAG_HPP_INCLUDED
#define EP_TAG_HPP_INCLUDED



namespace ep_lib
{
  int print_comm_info(MPI_Comm comm);



//  int MPI_Imrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message, MPI_Request *request);
//  int MPI_Mrecv(void *buf, int count, MPI_Datatype datatype, MPI_Message *message);


//  int MPI_Testall(int count, MPI_Request *array_of_requests, int *flag, MPI_Status *array_of_statuses);
//  int MPI_Testany(int count, MPI_Request *array_of_requests, int *index, int *flag, MPI_Status *status);
//  int MPI_Testsome(int incount, MPI_Request *array_of_requests, int *outcount, int *array_of_indices, MPI_Status *array_of_statuses);


//  int MPI_Waitany(int count, MPI_Request *array_of_requests,  int *index, MPI_Status *array_of_statuses);
//  int MPI_Waitsome(int incount, MPI_Request *array_of_requests, int *outcount, int *array_of_indices, MPI_Status *array_of_statuses);
}

#endif // EP_TAG_HPP_INCLUDED
