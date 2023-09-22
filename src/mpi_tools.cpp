#include "mpi.hpp"
#include "backtrace.hpp"
#include "exception.hpp"
#include "log.hpp"
#include <string>
#include <sstream>

namespace xios  
{
  std::map<MPI_Comm,string> CCommTrack::commTrack_ ;
  void CCommTrack::registerComm(const MPI_Comm& comm)
  {
    if (comm!=MPI_COMM_NULL)
    {
      auto it = commTrack_.find(comm) ;
      if (it == commTrack_.end())  commTrack_[comm] = MemTrack::backTrace(3);
      else ERROR("CCommtrack::registerComm", << "Communicator already allocated : " << endl<<it->second)
    }
  }

  void CCommTrack::releaseComm(const MPI_Comm& comm)
  {
    auto it = commTrack_.find(comm) ;
    if (it == commTrack_.end())  ERROR("CCommtrack::releaseComm", << "Communicator not allocated : " << endl)
    else commTrack_.erase(it) ;
  }

  void CCommTrack::dumpComm(void)
  {
    ostringstream ostr ;
    ostr<<" LIST of UNFREED MPI COMMUNICATORS"<<endl ;
    for(auto& it : commTrack_ )
    {
      ostr<<"---------------------------"<<endl ;
      ostr<<" -- unfreed communicator --"<<endl ;
      ostr<<" --       backtrace      --"<<endl ;
      ostr<<it.second<<endl ;
    }
    info(100)<<ostr.str() ;
  }

  int MPI_Bcast_string(std::string& str, int root, MPI_Comm comm) 
  {
    int commRank ;
    int ret ;
    MPI_Comm_rank(comm,&commRank) ;
    int strSize ;
    if (commRank==root) strSize=str.size() ;
    MPI_Bcast(&strSize,1,MPI_INT,root,comm) ;
  
    if (commRank==root) ret=MPI_Bcast((char*)str.data(), strSize, MPI_CHAR, root, comm) ;
    else
    {
      char* tmp=new char[strSize] ;
      ret=MPI_Bcast(tmp, strSize, MPI_CHAR, root, comm) ;
      str=std::string(tmp,strSize) ;
      delete [] tmp ;
    }
    return ret ;
  }

  template<>
  MPI_Datatype MPI_GetType<bool>(void) { return MPI_CXX_BOOL ;}

  template<>
  MPI_Datatype MPI_GetType<char>(void) { return MPI_CHAR ;}

  template<>
  MPI_Datatype MPI_GetType<short int>(void) { return MPI_SHORT ;}

  template<>
  MPI_Datatype MPI_GetType<int>(void) { return MPI_INT ;}

  template<>
  MPI_Datatype MPI_GetType<size_t>(void) { return MPI_SIZE_T ;}

  template<>
  MPI_Datatype MPI_GetType<float>(void) { return MPI_FLOAT ;}

  template<>
  MPI_Datatype MPI_GetType<double>(void) { return MPI_DOUBLE ;}

  template<>
  MPI_Datatype MPI_GetType<long double>(void) { return MPI_LONG_DOUBLE ;}

}
