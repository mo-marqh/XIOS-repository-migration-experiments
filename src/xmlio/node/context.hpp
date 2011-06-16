#ifndef __XMLIO_CContext__
#define __XMLIO_CContext__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "node_type.hpp"
#include "calendar.hpp"

#include "declare_group.hpp"

namespace xmlioserver {
namespace data {
    class CDataTreatment;
} // namespace tree
} // namespace xmlioserver

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Déclarations ////////////////////// ///
   class CContextGroup;
   class CContextAttributes;
   class CContext;

   ///--------------------------------------------------------------

   // Declare/Define CFileAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CContext)
#  include "context_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CContext)

   ///--------------------------------------------------------------

   class CContext
      : public CObjectTemplate<CContext>
      , public CContextAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CContext>   SuperClass;
         typedef CContextAttributes SuperClassAttribute;

      public :

         typedef CContextAttributes RelAttributes;
         typedef CContext           RelGroup;

         //---------------------------------------------------------

      public :

         /// Constructeurs ///
         CContext(void);
         explicit CContext(const StdString & id);
         CContext(const CContext & context);       // Not implemented yet.
         CContext(const CContext * const context); // Not implemented yet.

         /// Destructeur ///
         virtual ~CContext(void);

         //---------------------------------------------------------

      public :
      
         /// Mutateurs ///
         void setCalendar(boost::shared_ptr<date::CCalendar> newCalendar);
         void setDataTreatment(boost::shared_ptr<data::CDataTreatment> datat);
      
         /// Accesseurs ///
         boost::shared_ptr<date::CCalendar>      getCalendar(void) const;
         boost::shared_ptr<data::CDataTreatment> getDataTreatment(void) const;

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);         
         static ENodeType GetType(void);         

         static boost::shared_ptr<CContextGroup> GetContextGroup(void);

      public :

         /// Traitements ///
         virtual void solveDescInheritance(const CAttributeMap * const parent = 0);
         void solveFieldRefInheritance(void);
         void solveCalendar(void);

         /// Autres méthodes statiques ///
         static void ShowTree(StdOStream & out = std::clog);
         static void CleanTree(void);

         /// Test ///
         virtual bool hasChild(void) const;
         
      public :
      
         /// Autres ///
         virtual void parse(xml::CXMLNode & node);

         virtual StdString toString(void) const;
         virtual void toBinary  (StdOStream & os) const;
         virtual void fromBinary(StdIStream & is);
         
      private :
      
         boost::shared_ptr<date::CCalendar>      calendar;
         boost::shared_ptr<data::CDataTreatment> datat;

   }; // class CContext

   ///--------------------------------------------------------------

   // Declare/Define CContextGroup and CContextDefinition
   DECLARE_GROUP(CContext);

   ///--------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CContext__
