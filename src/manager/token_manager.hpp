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
        const MPI_Aint windowSize=sizeof(size_t);
        MPI_Win_allocate(windowSize, 1, MPI_INFO_NULL, comm, &winBufferCurrent_,   &winCurrentToken_) ;
        MPI_Win_allocate(windowSize, 1, MPI_INFO_NULL, comm, &winBufferRetrieved_, &winRetrievedToken_) ;
        if (leader_== commRank) {
          memset(   winBufferCurrent_, 0, windowSize );
          memset( winBufferRetrieved_, 0, windowSize );
        }
        MPI_Win_lock_all(0, winCurrentToken_) ;
        MPI_Win_lock_all(0, winRetrievedToken_) ;
      }
      
      ~CTokenManager()
      {
        MPI_Win_unlock_all(winCurrentToken_) ;
        MPI_Win_unlock_all(winRetrievedToken_) ;
        MPI_Win_free(&winCurrentToken_) ;
        MPI_Win_free(&winRetrievedToken_) ;
      }
      
      size_t getToken(void)
      {
        size_t inc=1 ;
        size_t token ;
        MPI_Fetch_and_op(&inc, &token, MPI_SIZE_T, leader_, 0, MPI_SUM, winCurrentToken_) ;
        MPI_Win_flush(leader_, winCurrentToken_);
        return token ;
      }

      bool checkToken(size_t token)
      {
        size_t tokenRead ;
        size_t inc=0 ;
        MPI_Fetch_and_op(&inc, &tokenRead, MPI_SIZE_T, leader_, 0, MPI_NO_OP, winRetrievedToken_) ;
        MPI_Win_flush(leader_, winRetrievedToken_);
        return tokenRead==token ;
      }
      
      void updateToken(size_t token)
      {
        size_t inc=1 ;
        size_t tokenRead ;
        MPI_Fetch_and_op(&inc, &tokenRead, MPI_SIZE_T, leader_, 0, MPI_SUM, winRetrievedToken_) ;
        MPI_Win_flush(leader_, winRetrievedToken_);
        if (token!=tokenRead)  ERROR("void CTokenManager::unlockToken(size_t token)",<<"Cannot release token="<<token<<
                                     " that is not corresponding to the locked token="<<tokenRead) ;     
      }
/*      void unlockToken(size_t token)
      {
        size_t inc=1 ;
        size_t tokenRead ;
        MPI_Win_lock(MPI_LOCK_EXCLUSIVE, leader_, 0, winRetrievedToken_) ;
        MPI_Fetch_and_op(&inc, &tokenRead, MPI_SIZE_T, leader_, 0, MPI_SUM, winRetrievedToken_) ;
        MPI_Win_unlock(leader_, winRetrievedToken_) ;
        
        if (token!=tokenRead)  ERROR("void CTokenManager::unlockToken(size_t token)",<<"Cannot release token="<<token<<
                                     " that is not corresponding to the locked token="<<tokenRead) ;     
      }
*/
    private:

      MPI_Win winCurrentToken_ ;
      void* winBufferCurrent_ ;
      MPI_Win winRetrievedToken_ ;
      void* winBufferRetrieved_ ;
      
      int leader_ ;

      size_t currentToken_=0 ;
      size_t retrievedToken_=0 ;


  } ;


}

#endif
