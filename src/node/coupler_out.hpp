#ifndef __XIOS_CCouplerOut__
#define __XIOS_CCouplerOut__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "field.hpp"
#include "declare_group.hpp"
#include "date.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "context_client.hpp"
#include "mpi.hpp"

namespace xios
{

   /// ////////////////////// Déclarations ////////////////////// ///

   class CCouplerOutGroup;
   class CCouplerOutAttributes;
   class CCouplerOut;

   class CGarbageCollector;

   ///--------------------------------------------------------------

   // Declare/Define CCouplerOutAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CCouplerOut)
#  include "coupler_out_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CCouplerOut)

   ///--------------------------------------------------------------

   /*!
   \class CCouplerOut
   
   */
   class CCouplerOut
      : public CObjectTemplate<CCouplerOut>
      , public CCouplerOutAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CCouplerOut>   SuperClass;
         typedef CCouplerOutAttributes SuperClassAttribute;

      public :
         enum EEventId
         {
           NO_EVENT
         };

         typedef CCouplerOutAttributes RelAttributes;
         typedef CCouplerOutGroup      RelGroup;

         /// Constructeurs ///
         CCouplerOut(void);
         explicit CCouplerOut(const StdString& id);
         CCouplerOut(const CCouplerOut& couplerOut);       // Not implemented yet.
         CCouplerOut(const CCouplerOut* const couplerOut); // Not implemented yet.

         /// Destructeur ///
         virtual ~CCouplerOut(void);

      public:
         /// Accesseurs ///
         CFieldGroup* getVirtualFieldGroup(void) const;
         std::vector<CField*> getAllFields(void) const;
         std::vector<CField* > getEnabledFields(void);

         StdString dumpClassAttributes(void);

      public :
         void setVirtualFieldGroup(CFieldGroup* newVFieldGroup);

         // Add component into coupler
         CField* addField(const string& id = "");
         CFieldGroup* addFieldGroup(const string& id = "");

      public:
         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         static ENodeType GetType(void);
      public:
         /// Traitements ///
         virtual void parse(xml::CXMLNode& node);
//         virtual StdString toString(void) const;

      public:

      private :
         CFieldGroup* virtualFieldGroup;

//         std::shared_ptr<CDataOutput> data_out;
//         std::shared_ptr<CDataInput> data_in;
         std::vector<CField*> enabledFields;


      public:

   }; // class CCouplerOut

   ///--------------------------------------------------------------

   // Declare/Define CFileGroup and CFileDefinition
   DECLARE_GROUP(CCouplerOut);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XIOS_CCouplingIn__