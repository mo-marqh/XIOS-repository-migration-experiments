#ifndef __TOKEN_MANAGER_HPP__
#define __TOKEN_MANAGER_HPP__

#include "xios_spl.hpp"
#include "exception.hpp"
#include "mpi.hpp"

namespace xios
{

  class CTokenManager
  {

    public:
      CTokenManager(MPI_Comm& comm, int leader) : leader_(leader)
      {
        int commRank ;
        MPI_Comm_rank(comm, &commRank) ;
        MPI_Aint size = 0 ;
        if (leader_== commRank) size = sizeof(size_t) ;
        MPI_Win_create(&currentToken_, size, sizeof(size_t), MPI_INFO_NULL, comm, &winCurrentToken_) ;
        MPI_Win_create(&retrievedToken_, size, sizeof(size_t), MPI_INFO_NULL, comm, &winRetrievedToken_) ;
      }

      size_t getToken(void)
      {
        size_t inc=1 ;
        size_t token ;
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, leader_, 0, winCurrentToken_) ;
        MPI_Fetch_and_op(&inc, &token, MPI_SIZE_T, leader_, 0, MPI_SUM, winCurrentToken_) ;
        MPI_Win_unlock(leader_, winCurrentToken_) ;
        return token ;
      }

      bool lockToken(size_t token)
      {
        size_t tokenRead ;
        MPI_Win_lock(MPI_LOCK_SHARED, leader_, 0, winRetrievedToken_) ;
        MPI_Get(&tokenRead, 1, MPI_SIZE_T, leader_, 0, 1, MPI_SIZE_T, winRetrievedToken_ ) ;
        MPI_Win_unlock(leader_, winRetrievedToken_) ;
        if (token==tokenRead) return true ;
        else return false ;
      }

      void unlockToken(size_t token)
      {
        size_t inc=1 ;
        size_t tokenRead ;
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, leader_, 0, winRetrievedToken_) ;
        MPI_Fetch_and_op(&inc, &tokenRead, MPI_SIZE_T, leader_, 0, MPI_SUM, winRetrievedToken_) ;
        MPI_Win_unlock(leader_, winRetrievedToken_) ;
        
        if (token!=tokenRead)  ERROR("void CTokenManager::unlockToken(size_t token)",<<"Cannot release token="<<token<<
                                     " that is not corresponding to the locked token="<<tokenRead) ;     
      }

    private:

      MPI_Win winCurrentToken_ ;
      MPI_Win winRetrievedToken_ ;
      
      int leader_ ;

      size_t currentToken_=0 ;
      size_t retrievedToken_=0 ;


  } ;


}

#endif