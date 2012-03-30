#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "base_type.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

namespace xios
{
   CBaseType::CBaseType(void) {}
   
   void CBaseType::fromString(const string& str)
   { 
   
      ERROR("void CBaseType::fromString(const string& str)",
            <<"Non implemented method in derived class");
   }

   string CBaseType::toString(void) const
   {
      ERROR("string CBaseType::toString(void) const",
            <<"Non implemented method in derived class");
      return string("") ;
   }
   
   bool CBaseType::fromBuffer(CBufferIn& buffer)
   { 
   
      ERROR("bool CBaseType::fromBuffer(CBufferIn& buffer) const",
            <<"Non implemented method in derived class");
      return false ;
   }

   bool CBaseType::toBuffer(CBufferOut& buffer) const
   {
      ERROR("bool CBaseType::fromBuffer(CBufferIn& buffer) const",
            <<"Non implemented method in derived class");
      return false ;
   }
   
   CBaseType* CBaseType::duplicate(void)
   {
     return this ;
   }

   CBaseType* CBaseType::duplicate(void) const 
   {
      ERROR(" CBaseType* CBaseType::duplicate(void) const ",
            <<"Non implemented method in derived class");
     return 0 ;
   }
   
   void CBaseType::destroy(void)
   {
     
   }

    size_t CBaseType::size(void) const
   {
      ERROR("size_t CBaseType::size(void) const) const",
            <<"Non implemented method in derived class");
      return 0 ;
   }

}   
