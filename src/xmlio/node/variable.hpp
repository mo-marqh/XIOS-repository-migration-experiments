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

            /// Accesseurs statiques ///
            static StdString GetName(void);
            static StdString GetDefName(void);
            static ENodeType GetType(void);

      }; // class CVar

      ///--------------------------------------------------------------

      // Declare/Define CVarGroup and CVarDefinition
      DECLARE_GROUP(CVariable);



   } // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CVariable__
