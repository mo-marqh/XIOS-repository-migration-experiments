#include "ep_lib.hpp"
#include <mpi.h>
#include "ep_declaration.hpp"


namespace ep_lib
{



  int MPI_Init_thread(int *argc, char*** argv, int required, int*provided)
  {
    printf("MPI_Init_thread\n");

    int id = omp_get_thread_num();

    if(id == 0)
    {
      ::MPI_Init_thread(argc, argv, required, provided);
    }
    return 0;
  }

  int MPI_Init(int *argc, char ***argv)
  {
    printf("MPI_init called\n");
    int id = omp_get_thread_num();

    if(id == 0)
    {
      ::MPI_Init(argc, argv);
    }
  	return 0;
  }

  int MPI_Initialized(int *flag)
  {
    printf("MPI_initialized called\n");

    ::MPI_Initialized(flag);
    
    return 0;
  }



}


