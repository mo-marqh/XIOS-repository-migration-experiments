#ifndef __XIOS_TYPE_IMPL__
#define __XIOS_TYPE_IMPL__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "message.hpp"



namespace xmlioserver
{
  using namespace std;
  
  template <typename T>
  CType<T>::CType(const T& val)
  {
    value=val ;
    ptrValue=&value ;
    clone=false ;
  }
  
  template <typename T>
  CType<T>::CType(T& val)
  {
    ptrValue=&val ;
    clone=false ;
  }

  template <typename T>
  CType<T>::CType(const CType<T>& type)
  {
    if (type.ptrValue==&type.value)
    {
       value=type.value ;
       ptrValue=&value ;
    }
    else 
    {
      ptrValue=type.ptrValue ;
    }
  }
  

  
  template <typename T>
  void CType<T>::fromString(const string& str)
  {
    istringstream iss(str);
    checkAccess() ;
    iss>>*ptrValue ;
    
  }

  template <typename T>
  size_t CType<T>::size(void) const
  {
    return sizeof(T) ;
  }
  

  template <typename T>
  string CType<T>::toString(void) const
  {
    ostringstream oss;
    checkAccess() ;
    oss<<*ptrValue ;
    return oss.str() ;
  }
  
  template <typename T>
  bool CType<T>::toBuffer(CBufferOut& buffer) const
  {
    checkAccess() ;
    return buffer.put(*ptrValue) ;
  }
  
  template <typename T>
  bool CType<T>::fromBuffer(CBufferIn& buffer)
  {
    checkAccess() ;
    return buffer.get(*ptrValue) ;
  }
 
     
  template <typename T>
  void CType<T>::checkAccess(void) const
  {
//    if (!isAssign) ERROR("void CType<T>::checkAccess",<<"CType<type> has not been assign to any buffer") ;
  }

  template <typename T>
  CBaseType* CType<T>::duplicate(void) const
  {
    
    CType<T>* ret= new CType<T>(*this) ;
    ret->clone=true ;
    return ret ;
  }
  
  template <typename T>
  void CType<T>::destroy(void)
  {
    if (clone) delete this ;
  }
  

  
  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const CType<T>& type)
  {
    if (!type.toBuffer(buffer)) ERROR("CBuffer& operator<<(CBuffer& buffer, CType<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }

  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, T& type)
  {
    if (!CType<T>(type).toBuffer(buffer)) ERROR("operator<<(CBuffer& buffer, T& type)",
                                           <<"Buffer remain size is to low for size type") ;      
    return buffer ;
  }

  template <typename T>
  CBufferOut& operator<<(CBufferOut& buffer, const T& type)
  {
    if (!CType<T>(type).toBuffer(buffer)) ERROR("operator<<(CBuffer& buffer, const T& type)",
                                           <<"Buffer remain size is to low for size type") ;      
    return buffer ;
  }
  
  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, const CType<T>& type)
  {
    if (! const_cast<CType<T> & >(type).fromBuffer(buffer)) ERROR("CBuffer& operator<<(CBuffer& buffer, CType<T>& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }
  
  template <typename T>
  CBufferIn& operator>>(CBufferIn& buffer, T& type)
  {
    if (!CType<T>(type).fromBuffer(buffer)) ERROR(" CBuffer& operator>>(CBuffer& buffer, T& type)",
                                           <<"Buffer remain size is to low for size type") ;
    return buffer ;
  }



  template <typename T>
  CMessage& operator<<(CMessage& msg, const CType<T>& type)
  {
    msg.push(*type.duplicate()) ;
    return msg ;
  }

  template <typename T>
  CMessage& operator<<(CMessage& msg,CType<T>& type)
  {
    msg.push(type) ;
    return msg ;
  }

  template <typename T>
  CMessage& operator<<(CMessage& msg, const T& type)
  {
    msg.push(*CType<T>(type).duplicate()) ;
    return msg ;
  }
  
  template <typename T>
  CMessage& operator<<(CMessage& msg, T& type)
  {
    msg.push(*CType<T>(type).duplicate()) ;
    return msg ;
  }

}

#endif
