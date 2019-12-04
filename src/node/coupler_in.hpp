#ifndef __XIOS_CCouplerIn__
#define __XIOS_CCouplerIn__

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

   class CCouplerInGroup;
   class CCouplerInAttributes;
   class CCouplerIn;

   class CGarbageCollector;

   ///--------------------------------------------------------------

   // Declare/Define CCouplerInAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CCouplerIn)
#  include "coupler_in_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CCouplerIn)

   ///--------------------------------------------------------------

   /*!
   \class CCouplerIn
   This class corresponds to file component of the xml.
   The class contains all the nessceary information to write data into a netcdf file: The most important thing
   is the field(s) which will be written into file. Besides, there are some options to write
   data into file, e.g: writting into only one file or multiple file; splitting a running into several files.
   Moreover, there are some other attributes of netcdf file which are also stored in this class
   */
   class CCouplerIn
      : public CObjectTemplate<CCouplerIn>
      , public CCouplerInAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CCouplerIn>   SuperClass;
         typedef CCouplerInAttributes SuperClassAttribute;

      public :
         enum EEventId
         {
           NO_EVENT
         };

         typedef CCouplerInAttributes RelAttributes;
         typedef CCouplerInGroup      RelGroup;

         /// Constructeurs ///
         CCouplerIn(void);
         explicit CCouplerIn(const StdString& id);
         CCouplerIn(const CCouplerIn& couplerIn);       // Not implemented yet.
         CCouplerIn(const CCouplerIn* const couplerIn); // Not implemented yet.

         /// Destructeur ///
         virtual ~CCouplerIn(void);

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

   }; // class CCouplerIn

   ///--------------------------------------------------------------

   // Declare/Define CCouplerInGroup and CCouplerInDefinition
   DECLARE_GROUP(CCouplerIn);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XIOS_CCouplingIn__