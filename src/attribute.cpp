#include "attribute.hpp"
#include "base_type.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CAttribute::CAttribute(const StdString & id)
         : CObject(id), CBaseType()
         , value()
      { /* Ne rien faire de plus */ }

      CAttribute::CAttribute(const CAttribute & attribut)
         : CObject(attribut.getId()),CBaseType()
      { 
         this->value = attribut.getAnyValue(); 
      }
      
      CAttribute::~CAttribute(void)
      { /* Ne rien faire de plus */ }
      
      ///--------------------------------------------------------------

      const boost::any & CAttribute::getAnyValue(void) const
      { 
         return (this->value); 
      }


      void CAttribute::setAnyValue(const boost::any & value)
      { 
         this->value = value; 
      }
      
      void CAttribute::clear(void)
      {
         this->value = boost::any(); 
      }

      //---------------------------------------------------------------

      bool CAttribute::isEmpty(void) const
      { 
         return (this->value.empty()); 
      }

      const StdString & CAttribute::getName(void) const
      { 
         return (this->getId()); 
      }
      

      ///--------------------------------------------------------------

   } // namespace tree
      
      CMessage& operator<<(CMessage& msg,tree::CAttribute& type)
      {
        msg.push(type) ;
        return msg ;
      }

     CMessage& operator<<(CMessage& msg, const tree::CAttribute&  type)
     {
       msg.push(*type.duplicate()) ;
       return msg ;
     }
 
      CBufferOut& operator<<(CBufferOut& buffer, tree::CAttribute&  type)
     {
    
       if (!type.toBuffer(buffer)) ERROR("CBufferOut& operator<<(CBufferOut& buffer, tree::CAttribute&  type)",
                                           <<"Buffer remain size is to low for size type") ;
      return buffer ;
     }
     
     CBufferIn& operator>>(CBufferIn& buffer, tree::CAttribute&  type)
     {
    
       if (!type.fromBuffer(buffer)) ERROR("CBufferInt& operator>>(CBufferIn& buffer, tree::CAttribute&  type)",
                                           <<"Buffer remain size is to low for size type") ;
       return buffer ;
     }

} // namespace xmlioserver
