#include "context.hpp"

#include "tree_manager.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "calendar_type.hpp"

#include "data_treatment.hpp"

namespace xmlioserver {
namespace tree {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CContext::CContext(void)
      : CObjectTemplate<CContext>(), CContextAttributes()
      , calendar()
   { /* Ne rien faire de plus */ }

   CContext::CContext(const StdString & id)
      : CObjectTemplate<CContext>(id), CContextAttributes()
      , calendar()
   { /* Ne rien faire de plus */ }

   CContext::~CContext(void)
   { /* Ne rien faire de plus */ }

   //----------------------------------------------------------------

   StdString CContext::GetName(void)   { return (StdString("context")); }
   StdString CContext::GetDefName(void){ return (CContext::GetName()); }
   ENodeType CContext::GetType(void)   { return (eContext); }

   //----------------------------------------------------------------

   boost::shared_ptr<CContextGroup> CContext::GetContextGroup(void)
   {  
      static boost::shared_ptr<CContextGroup> group_context
                          (new CContextGroup(xml::CXMLNode::GetRootName()));
      return (group_context); 
   }
   
   //----------------------------------------------------------------
   void CContext::setDataTreatment(boost::shared_ptr<data::CDataTreatment> ndatat)
   {
      this->datat = ndatat;
   }

   //----------------------------------------------------------------
   boost::shared_ptr<data::CDataTreatment> CContext::getDataTreatment(void) const
   {
      return (this->datat);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<date::CCalendar> CContext::getCalendar(void) const
   {
      return (this->calendar);
   }
   
   //----------------------------------------------------------------
   
   void CContext::setCalendar(boost::shared_ptr<date::CCalendar> newCalendar)
   {
      this->calendar = newCalendar;
      calendar_type.setValue(this->calendar->getId());
      start_date.setValue(this->calendar->getInitDate().toString());
   }
   
   //----------------------------------------------------------------

   void CContext::solveCalendar(void)
   {
      if (this->calendar.get() != NULL) return;
      if (calendar_type.isEmpty() || start_date.isEmpty())
         ERROR(" CContext::solveCalendar(void)",
               << "[ context id = " << this->getId() << " ] "
               << "Impossible de définir un calendrier (un attribut est manquant).");

#define DECLARE_CALENDAR(MType  , mtype)                        \
   if (calendar_type.getValue().compare(#mtype) == 0)           \
   {                                                            \
      this->calendar =  boost::shared_ptr<date::CCalendar>      \
         (new date::C##MType##Calendar(start_date.getValue())); \
      return;                                                   \
   }
#include "calendar_type.conf"

      ERROR("CContext::solveCalendar(void)",
            << "[ calendar_type = " << calendar_type.getValue() << " ] "
            << "Le calendrier n'est pas définie dans le code !");
   }
   
   //----------------------------------------------------------------

   void CContext::parse(xml::CXMLNode & node)
   {
      CContext::SuperClass::parse(node);

      // PARSING POUR GESTION DES ENFANTS
      xml::THashAttributes attributes;

      if (node.getElementName().compare(CContext::GetName()))
         DEBUG("Le noeud est mal nommé mais sera traité comme un contexte !");

      if (!(node.goToChildElement()))
      {
         DEBUG("Le context ne contient pas d'enfant !");
      }
      else
      {
         do { // Parcours des contextes pour traitement.

            StdString name = node.getElementName();
            attributes.clear();
            attributes = node.getAttributes();

            if (attributes.end() != attributes.find("id"))
            { DEBUG(<< "Le noeud de définition possède un identifiant,"
                    << " ce dernier ne sera pas pris en compte lors du traitement !"); }

#define DECLARE_NODE(Name_, name_)    \
   if (name.compare(C##Name_##Definition::GetDefName()) == 0) \
   { CObjectFactory::CreateObject<C##Name_##Definition>(C##Name_##Definition::GetDefName()) -> parse(node); \
   continue; }
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
            std::cout << name << std::endl;
            DEBUG(<< "L'élément nommé \'"     << name
                  << "\' dans le contexte \'" << CObjectFactory::GetCurrentContextId()
                  << "\' ne représente pas une définition !");

         } while (node.goToNextElement());

         node.goToParentElement(); // Retour au parent
      }
   }

   //----------------------------------------------------------------

   void CContext::ShowTree(StdOStream & out)
   {
      StdString currentContextId =
         CObjectFactory::GetCurrentContextId();
      std::vector<boost::shared_ptr<CContext> > def_vector =
         CContext::GetContextGroup()->getChildList();
      std::vector<boost::shared_ptr<CContext> >::iterator
         it = def_vector.begin(), end = def_vector.end();

      out << "<? xml version=\"1.0\" ?>" << std::endl;
      out << "<"  << xml::CXMLNode::GetRootName() << " >" << std::endl;
      
      for (; it != end; it++)
      {
         boost::shared_ptr<CContext> context = *it;         
         CTreeManager::SetCurrentContextId(context->getId());         
         out << *context << std::endl;
      }
      
      out << "</" << xml::CXMLNode::GetRootName() << " >" << std::endl;
      CTreeManager::SetCurrentContextId(currentContextId);  
   }
   
   //----------------------------------------------------------------
   
   void CContext::toBinary(StdOStream & os) const
   {
      SuperClass::toBinary(os);
       
#define DECLARE_NODE(Name_, name_)                                         \
   {                                                                       \
      ENodeType renum = C##Name_##Definition::GetType();                   \
      bool val = CObjectFactory::HasObject<C##Name_##Definition>           \
                     (C##Name_##Definition::GetDefName());                 \
      os.write (reinterpret_cast<const char*>(&renum), sizeof(ENodeType)); \
      os.write (reinterpret_cast<const char*>(&val), sizeof(bool));        \
      if (val) CObjectFactory::GetObject<C##Name_##Definition>             \
                     (C##Name_##Definition::GetDefName())->toBinary(os);   \
   }   
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
   
   //----------------------------------------------------------------
   
   void CContext::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
#define DECLARE_NODE(Name_, name_)                                         \
   {                                                                       \
      bool val = false;                                                    \
      ENodeType renum = Unknown;                                           \
      is.read (reinterpret_cast<char*>(&renum), sizeof(ENodeType));        \
      is.read (reinterpret_cast<char*>(&val), sizeof(bool));               \
      if (renum != C##Name_##Definition::GetType())                        \
         ERROR("CContext::fromBinary(StdIStream & is)",                    \
               << "[ renum = " << renum << "] Bad type !");                \
      if (val) CObjectFactory::CreateObject<C##Name_##Definition>          \
                   (C##Name_##Definition::GetDefName()) -> fromBinary(is); \
   }   
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
      
   }
   
   
   //----------------------------------------------------------------

   StdString CContext::toString(void) const
   {
      StdOStringStream oss;
      oss << "<" << CContext::GetName()
          << " id=\"" << this->getId() << "\" "
          << SuperClassAttribute::toString() << ">" << std::endl;
      if (!this->hasChild())
      {
         //oss << "<!-- No definition -->" << std::endl; // fait planter l'incrémentation
      }
      else
      {

#define DECLARE_NODE(Name_, name_)    \
   if (CObjectFactory::HasObject<C##Name_##Definition>(C##Name_##Definition::GetDefName())) \
   oss << *CObjectFactory::GetObject<C##Name_##Definition>(C##Name_##Definition::GetDefName()) << std::endl;
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"

      }

      oss << "</" << CContext::GetName() << " >";

      return (oss.str());
   }

   //----------------------------------------------------------------

   void CContext::solveDescInheritance(const CAttributeMap * const UNUSED(parent))
   {
#define DECLARE_NODE(Name_, name_)    \
   if (CObjectFactory::HasObject<C##Name_##Definition>(C##Name_##Definition::GetDefName())) \
   CObjectFactory::GetObject<C##Name_##Definition>(C##Name_##Definition::GetDefName())->solveDescInheritance();
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }

   //----------------------------------------------------------------

   bool CContext::hasChild(void) const
   {
      return (
#define DECLARE_NODE(Name_, name_)    \
   CObjectFactory::HasObject<C##Name_##Definition>  (C##Name_##Definition::GetDefName())   ||
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
      false);
}

   //----------------------------------------------------------------

   void CContext::solveFieldRefInheritance(void)
   {
      if (!this->hasId()) return;
      std::vector<boost::shared_ptr<CField> > allField
               = CObjectTemplate<CField>::GetAllVectobject(this->getId());
      std::vector<boost::shared_ptr<CField> >::iterator 
         it = allField.begin(), end = allField.end();
            
      for (; it != end; it++)
      {
         boost::shared_ptr<CField> field = *it;
         field->solveRefInheritance();
      }
   }

   //----------------------------------------------------------------

   void CContext::CleanTree(void)
   {
#define DECLARE_NODE(Name_, name_) C##Name_##Group::ClearAllAttributes();
#define DECLARE_NODE_PAR(Name_, name_)
#include "node_type.conf"
   }
   ///---------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver
