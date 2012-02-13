#ifndef __XIOS_TYPE__
#define __XIOS_TYPE__

#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"
#include "base_type.hpp"


namespace xmlioserver
{

  template <typename T> 
  class CType : public CBaseType
  {
    public:
  
    CType(void) ;
    CType(const T& val) ;
    CType(T& val) ;
    CType(const CType& type) ;

    ~CType() {} ;

    void fromString(const string& str) ;
    string toString(void) const;
    
    bool fromBuffer(CBufferIn& buffer) ;
    bool toBuffer(CBufferOut& buffer) const;
    void destroy(void) ;
    
    size_t size(void) const;
    CBaseType* duplicate(void) const;  

    void checkAccess(void) const;
    T* ptrValue ;
    
    bool clone ;
    T value ; 
  } ;

}

#include "type_impl.hpp"
#include "type_specialisation.hpp"

#endif
