#ifndef __FIELD_ATTRIBUT__
#define __FIELD_ATTRIBUT__

#include "declare_attribut.hpp"
#include "attribut_registrar.hpp"

namespace XMLIOSERVER
{
   class FieldAttribut : public virtual AttributRegistrar
   {
      public :
      
         DECLARE_ATTR(name, string) ;
         DECLARE_ATTR(description, string) ; 
         DECLARE_ATTR(unit, string) ;
         DECLARE_ATTR(operation, string);
      
         DECLARE_ATTR(freq_op, int) ;
         DECLARE_ATTR(level, int) ;
         DECLARE_ATTR(prec, int) ;
         
         DECLARE_ATTR(src, string) ; // TEMPORAIRE, pour fieldgroup uniquement
      
         DECLARE_ATTR(enabled, bool);
      
         DECLARE_ATTR(axis_ref, string);
         DECLARE_ATTR(grid_ref, string);
         DECLARE_ATTR(zoom_ref, string);
         DECLARE_ATTR(field_ref, string);
      
         FieldAttribut(void)
         { registerAllAttributes(); }
                         
         void registerAllAttributes(void)
         {
            RegisterAttribut(&name) ;
            RegisterAttribut(&description) ; 
            RegisterAttribut(&unit) ;
            RegisterAttribut(&operation);
            
            RegisterAttribut(&src);// TEMPORAIRE, pour fieldgroup uniquement
      
            RegisterAttribut(&freq_op) ;
            RegisterAttribut(&level) ;
            RegisterAttribut(&prec) ;
            RegisterAttribut(&enabled);
      
            RegisterAttribut(&axis_ref);
            RegisterAttribut(&grid_ref);
            RegisterAttribut(&zoom_ref);
            RegisterAttribut(&field_ref);
         }
     
   } ; // class CFieldAttribut
   
}// namespace XMLIOSERVER

#endif //__FIELD_ATTRIBUT__
