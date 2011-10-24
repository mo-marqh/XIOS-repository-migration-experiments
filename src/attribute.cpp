#include "attribute.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CAttribute::CAttribute(const StdString & id)
         : CObject(id)
         , value()
      { /* Ne rien faire de plus */ }

      CAttribute::CAttribute(const CAttribute & attribut)
         : CObject(attribut.getId())
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
} // namespace xmlioserver
