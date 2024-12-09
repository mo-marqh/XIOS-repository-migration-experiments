#ifndef __BUFFER_CS_BASE_HPP__
#define __BUFFER_CS_BASE_HPP__

namespace xios
{
  class CBufferClientServerBase
  {
    protected:

      static const int headerSize_= 4*sizeof(size_t);
      static const int timeLineOffset_ = 0 ; // in size_t
      static const int countOffset_    = 1 ; // in size_t
      static const int controlOffset_  = 2 ; // in size_t
      static const int notifyOffset_   = 3 ; // in size_t

      static const int notifyNothing_ = 0 ;
      static const int notifyFinalize_ = 1 ;
      static const int notifyResizeBuffer_ = 2 ;

  } ;

}

#endif