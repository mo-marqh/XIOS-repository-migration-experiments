#ifndef __BASE_TYPE_HPP__
#define __BASE_TYPE_HPP__

#include "xmlioserver_spl.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

namespace xios
{

  class CBaseType
  {
    public:
    
    CBaseType(void) ;
    virtual void fromString(const string& str) ;
    virtual string toString(void) const;
    
    virtual bool fromBuffer(CBufferIn& buffer) ;
    virtual bool toBuffer(CBufferOut& buffer) const;
    virtual CBaseType* duplicate(void) const;
    virtual CBaseType* duplicate(void) ;
    virtual void destroy(void) ;
    virtual size_t size(void) const;
  } ;

}

#endif
