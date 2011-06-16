#ifndef __XMLIO_CObjectTemplate__
#define __XMLIO_CObjectTemplate__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "attribute_map.hpp"
#include "node_enum.hpp"

namespace xmlioserver
{
   /// ////////////////////// Déclarations ////////////////////// ///
   template <class T>
      class CObjectTemplate
         : public CObject
         , public virtual tree::CAttributeMap
   {

         /// Friend ///
         friend class CObjectFactory;

         /// Typedef ///
         typedef tree::CAttributeMap SuperClassMap;
         typedef CObject SuperClass;
         typedef T DerivedType;

      public :

         /// Autres ///
         virtual StdString toString(void) const;
         virtual void fromString(const StdString & str);

         virtual void toBinary  (StdOStream & os) const;
         virtual void fromBinary(StdIStream & is);

         virtual void parse(xml::CXMLNode & node);
         
         /// Accesseurs ///
         tree::ENodeType getType(void) const;

         /// Test ///
         virtual bool hasChild(void) const;

         /// Traitements ///
         virtual void solveDescInheritance(const CAttributeMap * const parent = 0);

         /// Traitement statique ///
         static void ClearAllAttributes(void);

         /// Accesseur statique ///
         static std::vector<boost::shared_ptr<DerivedType> > &
            GetAllVectobject(const StdString & contextId);

         /// Destructeur ///
         virtual ~CObjectTemplate(void);

      protected :

         /// Constructeurs ///
         CObjectTemplate(void);
         explicit CObjectTemplate(const StdString & id);
         CObjectTemplate(const CObjectTemplate<T> & object,
                         bool withAttrList = true, bool withId = true);
         CObjectTemplate(const CObjectTemplate<T> * const object); // Not implemented.

      private :

         /// Propriétés statiques ///
         static xios_map<StdString,
                xios_map<StdString,
                boost::shared_ptr<DerivedType> > > AllMapObj; 
         static xios_map<StdString,
                std::vector<boost::shared_ptr<DerivedType> > > AllVectObj;

   }; // class CObjectTemplate
} // namespace xmlioserver

#endif // __XMLIO_CObjectTemplate__
