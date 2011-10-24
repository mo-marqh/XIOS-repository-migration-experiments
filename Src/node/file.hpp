#ifndef __XMLIO_CFile__
#define __XMLIO_CFile__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "field.hpp"
#include "declare_group.hpp"

#include "data_output.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Déclarations ////////////////////// ///

   class CFileGroup;
   class CFileAttributes;
   class CFile;

   ///--------------------------------------------------------------

   // Declare/Define CFileAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CFile)
#  include "file_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CFile)

   ///--------------------------------------------------------------

   class CFile
      : public CObjectTemplate<CFile>
      , public CFileAttributes
   {
         /// typedef ///
         typedef CObjectTemplate<CFile>   SuperClass;
         typedef CFileAttributes SuperClassAttribute;

      public :

         typedef CFileAttributes RelAttributes;
         typedef CFileGroup      RelGroup;

         /// Constructeurs ///
         CFile(void);
         explicit CFile(const StdString & id);
         CFile(const CFile & file);       // Not implemented yet.
         CFile(const CFile * const file); // Not implemented yet.

         /// Accesseurs ///
         boost::shared_ptr<io::CDataOutput> getDataOutput(void) const;
         boost::shared_ptr<CFieldGroup> getVirtualFieldGroup(void) const;
         std::vector<boost::shared_ptr<CField> > getAllFields(void) const;

         std::vector<boost::shared_ptr<CField> >
            getEnabledFields(int default_outputlevel = 5,
                             int default_level = 1,
                             bool default_enabled = true);

      public :

         /// Mutateurs ///
         void setVirtualFieldGroup(boost::shared_ptr<CFieldGroup> newVFieldGroup);
         void setVirtualFieldGroup(const StdString & newVFieldGroupId);

         void initializeDataOutput(boost::shared_ptr<io::CDataOutput> dout);
         void close(void) ;
         
         /// Traitements ///
         virtual void solveDescInheritance(const CAttributeMap * const parent = 0);
         void solveFieldRefInheritance(void);
         void solveEFGridRef(void);
         void solveEFOperation(void);

         /// Destructeur ///
         virtual ~CFile(void);

         /// Autres ///
         virtual void parse(xml::CXMLNode & node);
         virtual StdString toString(void) const;
         
         virtual void toBinary  (StdOStream & os) const;
         virtual void fromBinary(StdIStream & is);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         
         static ENodeType GetType(void);

      private :

         /// Propriétés privées ///
         boost::shared_ptr<CFieldGroup> vFieldGroup;
         boost::shared_ptr<io::CDataOutput> data_out;
         std::vector<boost::shared_ptr<CField> > enabledFields;

   }; // class CFile

   ///--------------------------------------------------------------

   // Declare/Define CFileGroup and CFileDefinition
   DECLARE_GROUP(CFile);

   ///--------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CFile__
