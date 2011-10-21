#ifndef __XMLIO_CVariable__
#define __XMLIO_CVariable__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "declare_group.hpp"

#include "data_output.hpp"

namespace xmlioserver
{
   namespace tree
   {
      /// ////////////////////// Déclarations ////////////////////// ///

      class CVariableGroup;
      class CVariableAttributes;
      class CVariable;

      ///--------------------------------------------------------------

      // Declare/Define CVarAttribute
      BEGIN_DECLARE_ATTRIBUTE_MAP(CVariable)
#include "var_attribute.conf"
      END_DECLARE_ATTRIBUTE_MAP(CVariable)

      ///--------------------------------------------------------------

      class CVariable
         : public CObjectTemplate<CVariable>
         , public CVariableAttributes
      {
            /// typedef ///
            typedef CObjectTemplate<CVariable>   SuperClass;
            typedef CVariableAttributes SuperClassAttribute;

            friend class CVariableGroup;

         public :

            typedef CVariableAttributes RelAttributes;
            typedef CVariableGroup      RelGroup;

            /// Constructeurs ///
            CVariable(void);
            explicit CVariable(const StdString & id);
            CVariable(const CVariable & var);       // Not implemented yet.
            CVariable(const CVariable * const var); // Not implemented yet.

            /// Destructeur ///
            virtual ~CVariable(void);

         public :
         
            /// Autres ///
            virtual void parse(xml::CXMLNode & node);
            virtual StdString toString(void) const;

            virtual void toBinary  (StdOStream & os) const;
            virtual void fromBinary(StdIStream & is);

            /// Accesseur ///
            const StdString & getContent (void) const;

            
            template <typename T> inline T getData(void) const;
//            bool inline getData<bool>(void) const ;
            
            template <typename T, StdSize N>
               inline void getData(ARRAY(T, N) _data_array) const;

         public :
         
            /// Accesseurs statiques ///
            static StdString GetName(void);
            static StdString GetDefName(void);
            static ENodeType GetType(void);

         private :

            StdString content;

      }; // class CVar

     
      template <typename T>
      T CVariable::getData(void) const
      {
         T retval ;
         std::stringstream sstr(std::stringstream::in | std::stringstream::out); 
         sstr<<content ;
         sstr>>retval ;
         if (sstr.fail()) ERROR("CVariable::getdata()",
               << "Cannot convert string <" << content << "> into type required" );
         return retval ;
      } 

      template<>
      bool CVariable::getData<bool>(void) const
      {
         if (content.compare("true")==0 || content.compare(".true.")==0 || content.compare(".TRUE.")==0) return true ; 
         else if (content.compare("false")==0 || content.compare(".false.")==0 || content.compare(".FALSE.")==0) return false ; 
         else ERROR("CVariable::getdata()",
               << "Cannot convert string <" << content << "> into type required" );
         return false ;
      } 
      ///--------------------------------------------------------------

      // Declare/Define CVarGroup and CVarDefinition
      DECLARE_GROUP_PARSE_REDEF(CVariable);



   } // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CVariable__
