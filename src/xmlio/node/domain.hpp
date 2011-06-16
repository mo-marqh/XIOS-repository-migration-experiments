#ifndef __XMLIO_CDomain__
#define __XMLIO_CDomain__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CDomainGroup;
   class CDomainAttributes;
   class CDomain;

   ///--------------------------------------------------------------

   // Declare/Define CDomainAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CDomain)
#  include "domain_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CDomain)

   ///--------------------------------------------------------------

   class CDomain
      : public CObjectTemplate<CDomain>
      , public CDomainAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CDomain>   SuperClass;
         typedef CDomainAttributes SuperClassAttribute;

      public :

         typedef CDomainAttributes RelAttributes;
         typedef CDomainGroup      RelGroup;

         /// Constructeurs ///
         CDomain(void);
         explicit CDomain(const StdString & id);
         CDomain(const CDomain & domain);       // Not implemented yet.
         CDomain(const CDomain * const domain); // Not implemented yet.

         /// Vérifications ///
         void checkAttributes(void);

      private :

         void checkGlobalDomain(void);

         void checkLocalIDomain(void);
         void checkLocalJDomain(void);

         void checkMask(void);
         void checkDomainData(void);
         void checkCompression(void);
         
         void checkZoom(void);

         void completeMask(void);

      public :
      
         /// Autres ///
         virtual void fromBinary(StdIStream & is);

         /// Accesseurs ///
         ARRAY(int, 2) getLocalMask(void) const;
         
         const std::set<StdString> & getRelFiles(void) const;

         const std::vector<int> & getIBeginSub(void) const;
         const std::vector<int> & getIEndSub(void) const;
         const std::vector<int> & getJBeginSub(void) const;
         const std::vector<int> & getJEndSub(void) const;
         
         const std::vector<ARRAY(double, 1)> & getLonValueSub(void) const;
         const std::vector<ARRAY(double, 1)> & getLatValueSub(void) const;

         /// Test ///
         bool IsWritten(const StdString & filename) const;
         bool hasZoom(void) const;
         bool isEmpty(void) const;
         
      public :
      
         /// Mutateur ///
         void addRelFile(const StdString & filename);
         void completeLonLat(void);
         
         /// Destructeur ///
         virtual ~CDomain(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

       private :

         /// Proriétés protégées ///
         bool isChecked;
         ARRAY(int, 2) local_mask;
         std::set<StdString> relFiles;

         std::vector<int> ibegin_sub, iend_sub, jbegin_sub, jend_sub;
         std::vector<ARRAY(double, 1)> lonvalue_sub, latvalue_sub;

   }; // class CDomain

   ///--------------------------------------------------------------

   // Declare/Define CDomainGroup and CDomainDefinition
   DECLARE_GROUP(CDomain);

   ///--------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CDomain__
