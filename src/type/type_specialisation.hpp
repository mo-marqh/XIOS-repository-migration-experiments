#ifndef __XIOS_TYPE_SPECIALISATION_HPP__
#define __XIOS_TYPE_SPECIALISATION_HPP__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "array.hpp"

namespace xmlioserver
{


// template specialisation for string

  template <>
  size_t CType<string>::size() const
  {
    size_t typeSize=0 ;
    typeSize+=sizeof(size_t) ;
    typeSize+=ptrValue->size() ;
    return typeSize ;
  }
  
  template <>
  bool CType<string>::toBuffer(CBufferOut& buffer) const
  {
    if (buffer.remain()<size()) return false ;
    else
    {
      bool ret=true ;
      if (ret) ret&=buffer.put(ptrValue->size()) ;
      if (ret) ret&=buffer.put(ptrValue->data(),ptrValue->size()) ;
      return ret ;
    }
  }

  template <>
  bool CType<string>::fromBuffer(CBufferIn& buffer)
  {
    bool ret=true ;
    size_t typeSize ;
    if (ret) ret&=buffer.get(typeSize) ;
    
    char* str;
    str= (char*) buffer.ptr() ;
    if (ret) buffer.advance(typeSize) ;
    if (ret) *ptrValue=string(str,typeSize) ;
    
    return ret ;
  }  
  
  
  // template specialisation for CArray
/*
  template<>
  size_t CType< ARRAY(int, 1)>::size() const
  {
     return (*(this->ptrValue))->getSize() ;
  }
  
  template <>
  bool CType<ARRAY(int, 1)>::toBuffer(CBufferOut& buffer) const
  {
      return (*(this->ptrValue))->toBuffer(buffer) ;
  }

  template <>
  bool CType<ARRAY(int, 1)>::fromBuffer(CBufferIn& buffer) const
  {
    return (*(this->ptrValue))->fromBuffer(buffer) ;
  }

  template <>
  void CType<ARRAY(int, 1)>::fromString(const string& str) const
  {
 // to implement
  }

  template <>
  string CType<ARRAY(int, 1)>::toString(void) const
  {
 // to implement
   return string("") ;
 
  }
*/


/*
template<size_t numDim>
boost::detail::multi_array::extent_gen<numDim> getExtentNull(void) { return getExtentNull<numDim-1>()[0];}

template<>
boost::detail::multi_array::extent_gen<1> getExtentNull<1>(void) { return extents[0]; }
*/

#define CTYPE_ARRAY(ValueType,NumsDims)                                \  
  template<>                                                           \
  size_t CType< ARRAY(ValueType,NumsDims)>::size() const                           \
  {                                                                    \
     return (*(this->ptrValue))->getSize() ;                           \
  }                                                                    \
                                                                       \
  template <>                                                          \
  bool CType<ARRAY(ValueType,NumsDims)>::toBuffer(CBufferOut& buffer) const        \
  {                                                                    \
      return (*(this->ptrValue))->toBuffer(buffer) ;                   \
  }                                                                    \
                                                                       \
  template <>                                                          \
  bool CType<ARRAY(ValueType,NumsDims)>::fromBuffer(CBufferIn& buffer)       \
  {                                                                    \
    shared_ptr<CArray<ValueType, NumsDims> > tmp(new CArray<ValueType, NumsDims>() ) ; \
    *(this->ptrValue)=tmp ;\
    return (*(this->ptrValue))->fromBuffer(buffer) ;                   \
  }                                                                    \
                                                                       \
  template <>                                                          \
  void CType<ARRAY(ValueType,NumsDims)>::fromString(const string& str)       \
  {                                                                    \
 /* to implement */                                                    \
  }                                                                    \
                                                                       \
  template <>                                                          \
  string CType<ARRAY(ValueType,NumsDims)>::toString(void) const                    \
  {                                                                    \
 /* to implement */                                                    \
   return string("") ;                                                 \
  }

//CTYPE_ARRAY(double,1) 

//CTYPE_ARRAY(double,2) 


CTYPE_ARRAY(int,1)
CTYPE_ARRAY(int,2)
CTYPE_ARRAY(int,3)
CTYPE_ARRAY(bool,1) 
CTYPE_ARRAY(bool,2) 
CTYPE_ARRAY(bool,3) 
CTYPE_ARRAY(double,1) 
CTYPE_ARRAY(double,2) 
CTYPE_ARRAY(double,3) 
CTYPE_ARRAY(float,1) 
CTYPE_ARRAY(float,2) 
CTYPE_ARRAY(float,3) 

}  

#endif  
