#ifndef __P2P_CLIENT_BUFFER_HPP__
#define __P2P_CLIENT_BUFFER_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "mpi.hpp"
#include "cxios.hpp"
#include "event_client.hpp"
#include "p2p_cs_buffer_base.hpp"

namespace xios
{
  extern CLogType logProtocol ;

  class CP2pClientBuffer : public CP2pCSBufferBase
  {
    class CBuffer
    {
      char* buffer_ ;
      size_t start_ ;
      size_t end_ ;
      size_t count_ ;
      size_t size_ ;
      bool fixed_ ;
      char* window_ ;

      public:
        CBuffer(char* window, size_t size, bool fixed) : start_(size), end_(0), count_(0), size_(size), fixed_(fixed), window_(window) 
        { 
          size_t trueSize = (size/8 + 1)*8 + 8 ;  // seems to have some problem with OpenMPi/UCX when trying to tranfer 1 byte
                                                  // at the end of the buffer. Alignment problem ? Or simple bug ? Seems that allocate
                                                  // greater buffer solves the problem 
          MPI_Alloc_mem(trueSize, MPI_INFO_NULL, &buffer_) ;
          info(logProtocol)<<"Attach memory to windows : addr="<<(MPI_Aint)buffer_<<"   count="<<size<<endl ;

          //MPI_Win_attach(window_, buffer_, trueSize) ;
          
        }
        ~CBuffer() 
        { 
          if (count_>0) ERROR("COneSidedClientBuffer::~CBuffer()",<<"Try to delete buffer that is not empty"<<std::endl) ;
          //MPI_Win_detach(window_, buffer_) ;
          //MPI_Free_mem(buffer_) ;
          info(logProtocol)<<"Detach memory from windows : addr="<<(MPI_Aint)buffer_<<"   count="<<size_<<endl ;
        }
       
        void write(char** buffer, size_t& size, MPI_Aint& addr, size_t& start, size_t& count)
        {
          addr = 0 ;
          count = 0 ;
          if (end_ < start_)
          {
            if (start_-end_ >= size)
            {
              count=size ;
              size = 0 ;
              start=end_ ;
              MPI_Get_address(&buffer_[start], &addr) ;
              end_  += count ; 
              count_+= count ;
              memcpy(&buffer_[start], *buffer, count) ;
              *buffer+=count ;
            } 
            else
            {
              count = start_-end_ ;
              size -= count ;
              start=end_ ;
              MPI_Get_address(&buffer_[start], &addr) ;
              end_ = start_ ;
              count_+=count ;
              memcpy(&buffer_[start], *buffer, count) ;
              *buffer+=count ;
            }
          }
          else if ( end_> start_ )
          {
            if (size_-end_ >= size)
            {
              count = size ;
              size = 0;
              start=end_ ;
              MPI_Get_address(&buffer_[start], &addr) ;
              end_   += count ;
              count_ += count ;
              memcpy(&buffer_[start], *buffer, count) ;
              *buffer+=count ;
            }
            else
            {
              count = size_ - end_ ;
              size -= count ;
              start=end_ ;
              MPI_Get_address(&buffer_[start], &addr) ;
              end_ = 0 ;
              count_+= count ;
              memcpy(&buffer_[start], *buffer, count) ;
              *buffer+=count ;
            }
          }
          else if (end_==start_)
          {
            count = 0 ;
          }

          // check

          if (count!=0)
          {
            MPI_Aint startBufferAddr,endBufferAddr ;
            MPI_Get_address(&buffer_[0], &startBufferAddr) ;
            MPI_Get_address(&buffer_[size_-1], &endBufferAddr) ;

            if (addr<startBufferAddr || MPI_Aint_add(addr,count-1)>endBufferAddr)
            {
              ERROR("CP2pClientBuffer::CBuffer::write(char** buffer, size_t& size, MPI_Aint& addr, size_t& start, size_t& count)",<<" out of bounds"<<std::endl) ;
            }

          }
        }

        void free(size_t start, size_t count)
        {
          start_ = start+count-1 ;
          count_ -= count ;
        }

        size_t remain(void) { return size_-count_; }
        size_t getSize(void) { return size_ ;}
        size_t getCount(void) {return count_ ;}
        size_t isFixed(void) {return fixed_;}
    } ;
  
    public:
      CP2pClientBuffer(MPI_Comm& interComm, int serverRank, MPI_Comm& commSelf, MPI_Comm& interCommMerged, int intraServerRank) ;
      void newBuffer(size_t size, bool fixed) ;
      bool isBufferFree(size_t size) ;
      int writeBuffer(char* buff, size_t size) ;
      void freeBuffer(MPI_Aint addr) ;
      bool freeBloc(MPI_Aint addr) ;
      bool writeEvent(size_t timeLine, CEventClient& event) ;
      bool isEmpty(void) { return blocs_.empty() ;}
      bool isNotifiedFinalized(void) { eventLoop() ; return buffers_.empty() && isFinalized_ ;}
      void setFixed(size_t size) { fixed_=true ; fixedSize_=size ;}
      void setGrowable(double growingFactor) { fixed_= false ; growingFactor_=growingFactor;}
      void setGrowingFactor(double growingFactor) {growingFactor_=growingFactor;}
      void eventLoop(void) ;
      void sendTimelineEvent(size_t timeline, int nbSenders, int nbBlocs) ;
      void sendResizeBufferEvent(size_t timeline, size_t currentBufferSize_) ;
      void sendNewBuffer(void) ;
      void createWindow(MPI_Comm& commSelf, MPI_Comm& interCommMerged, int intraServerRank ) ;
      void listenFinalize(void) ;

    private :
      
      struct SBloc
      {
        MPI_Aint addr ;
        CBuffer* buffer ;
        size_t start ;
        int    count ;
        int    window ;
      } ;

      struct SRequest
      {
        CBufferOut* buffer ;
        MPI_Request mpiRequest ;
      } ;

      struct SBlocRequest
      {
        MPI_Aint addr ;
        MPI_Request mpiRequest ;
      } ;
      
      MPI_Aint* control_ ;

      MPI_Comm interComm_; 
      MPI_Comm winComm_ ;

      //MPI_Win window_ ;
      //vector<MPI_Win> windows_ ;
      vector<char*> windows_ ;
      vector<bool> usedWindows_ ;
      int currentWindow_ ;
      int maxWindows_ ;

      //MPI_Win winControl_ ;
      int serverRank_ ;

      MPI_Comm interCommMerged_;  
      int intraServerRank_ ; 

      std::list<CBuffer*> buffers_ ;
      std::list<SBloc> blocs_ ;
      std::list<SBloc>::iterator lastBlocEvent_ ;
      CBuffer* currentBuffer_=nullptr ;
      std::list<SRequest> requests_ ;

      std::list<SBlocRequest> sentBlocRequest_ ;
      MPI_Request finalizeRequest_ ;

      bool fixed_=false;
      size_t fixedSize_ = 0 ;
      size_t currentBufferSize_= 0  ;
      double growingFactor_ = 2. ; 
      MPI_Aint lastFreedBloc_=0 ;
      bool isFinalized_ = false ;

  } ;

}


#endif
