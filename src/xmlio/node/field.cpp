#include "field.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "node_type.hpp"
#include "calendar_util.hpp"
#include "xios_manager.hpp"

namespace xmlioserver{
namespace tree {
   
   /// ////////////////////// Définitions ////////////////////// ///

   CField::CField(void)
      : CObjectTemplate<CField>(), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , freq_operation(), freq_write()
      , nstep(0)
      , last_Write(), last_operation()
      , foperation()
      , data(new CArray<double, 1>(boost::extents[0]))
   { /* Ne rien faire de plus */ }

   CField::CField(const StdString & id)
      : CObjectTemplate<CField>(id), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , freq_operation(), freq_write()
      , nstep(0)
      , last_Write(), last_operation()
      , foperation()
      , data(new CArray<double, 1>(boost::extents[0]))
   { /* Ne rien faire de plus */ }

   CField::~CField(void)
   {
      this->grid.reset() ;
      this->file.reset() ;
      this->foperation.reset() ;
      this->data.reset() ;
   }

   //----------------------------------------------------------------

   bool CField::updateDataServer
      (const date::CDate & currDate,
       const std::deque<ARRAY(double, 1)> storedClient)
   {
      const date::CDate opeDate      = *last_operation + freq_operation;
      const date::CDate writeDate    = *last_Write     + freq_write; 
      
      if (opeDate <= currDate)
      {
         if (this->data->num_elements() != this->grid->storeIndex[0]->num_elements())
         {
            this->data->resize(boost::extents[this->grid->storeIndex[0] ->num_elements()]);
         }  
         ARRAY_CREATE(input, double, 1, [this->data->num_elements()]);
         this->grid->inputFieldServer(storedClient, input);          
         (*this->foperation)(input);
         *last_operation = currDate;
      }
      if (writeDate < (currDate + freq_operation))
      {
         this->foperation->final();
         this->incrementNStep();
         *last_Write = writeDate;
         return (true);        
      }
      return (false);
   }

   //----------------------------------------------------------------

   void CField::setRelFile(const boost::shared_ptr<CFile> _file)
   { 
      this->file = _file; 
   }

   //----------------------------------------------------------------

   StdString CField::GetName(void)   { return (StdString("field")); }
   StdString CField::GetDefName(void){ return (CField::GetName()); }
   ENodeType CField::GetType(void)   { return (eField); }

   //----------------------------------------------------------------

   boost::shared_ptr<CGrid> CField::getRelGrid(void) const
   { 
      return (this->grid); 
   }

   //----------------------------------------------------------------

   boost::shared_ptr<CFile> CField::getRelFile(void) const
   { 
      return (this->file);
   }
   
   StdSize CField::getNStep(void) const
   {
      return (this->nstep);
   }
   
   void CField::incrementNStep(void)
   {
      this->nstep++;
   }

   //----------------------------------------------------------------

   boost::shared_ptr<CField> CField::getDirectFieldReference(void) const
   {
      if (this->field_ref.isEmpty())
         return (this->getBaseFieldReference());

      if (! CObjectFactory::HasObject<CField>(this->field_ref.getValue()))
         ERROR("CField::getDirectFieldReference(void)",
               << "[ ref_name = " << this->field_ref.getValue() << "]"
               << " invalid field name !");

      return (CObjectFactory::GetObject<CField>(this->field_ref.getValue()));
   }

   //----------------------------------------------------------------

   const boost::shared_ptr<CField> CField::getBaseFieldReference(void) const
   { 
      return (baseRefObject); 
   }

   //----------------------------------------------------------------

   const std::vector<boost::shared_ptr<CField> > & CField::getAllReference(void) const 
   { 
      return (refObject);
   }

   //----------------------------------------------------------------

   const StdString & CField::getBaseFieldId(void) const
   { 
      return (this->getBaseFieldReference()->getId());
   }
   
   //----------------------------------------------------------------
   
   const date::CDuration & CField::getFreqOperation(void) const
   {
      return (this->freq_operation);
   }
   
   //----------------------------------------------------------------
   const date::CDuration & CField::getFreqWrite(void) const
   {
      return (this->freq_write);
   }
   
   //----------------------------------------------------------------
         
   boost::shared_ptr<func::CFunctor> CField::getFieldOperation(void) const
   {
      return (this->foperation);
   }

   //----------------------------------------------------------------

   bool CField::hasDirectFieldReference(void) const
   { 
      return (!this->field_ref.isEmpty()); 
   }
   
   //----------------------------------------------------------------
   
   ARRAY(double, 1) CField::getData(void) const
   {
      return(this->data);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<date::CDate> CField::getLastWriteDate(void) const
   {
      return(this->last_Write);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<date::CDate> CField::getLastOperationDate(void) const
   {
      return(this->last_operation);
   }

   //----------------------------------------------------------------

   void CField::solveRefInheritance(void)
   {
      std::set<CField *> sset;
      boost::shared_ptr<CField> refer_sptr;
      CField * refer_ptr = this;
      
      this->baseRefObject = CObjectFactory::GetObject<CField>(this);
      
      while (refer_ptr->hasDirectFieldReference())
      {
         refer_sptr = refer_ptr->getDirectFieldReference();
         refer_ptr  = refer_sptr.get();

         if(sset.end() != sset.find(refer_ptr))
         {
            DEBUG (<< "Dépendance circulaire stoppée pour l'objet de type CField sur "
                   << "\"" + refer_ptr->getId() + "\" !");
            break;
         }

         SuperClassAttribute::setAttributes(refer_ptr);
         sset.insert(refer_ptr);
         baseRefObject = refer_sptr;
//ym         refObject.push_back(refer_sptr);
      }
   }

   //----------------------------------------------------------------

   void  CField::solveOperation(void)
   {
      using namespace func;
      using namespace date;
       
      StdString id = this->getBaseFieldReference()->getId();
      boost::shared_ptr<CContext> _context =
         CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId());

      if (operation.isEmpty() || freq_op.isEmpty() || this->file->output_freq.isEmpty())
      {
         ERROR("CField::solveOperation(void)",
               << "[ id = " << id << "]"
               << "Impossible de définir une opération pour le champ !");
      }
      
      CDuration freq_offset_ = NoneDu;
      if (!freq_offset.isEmpty())
      {
         freq_offset_ = CDuration::FromString(freq_offset.getValue());
      }
      else
      {
         freq_offset.setValue(NoneDu.toString());
      }  

      if (CXIOSManager::GetStatus() == CXIOSManager::LOC_SERVER)
      {
         this->freq_operation =
             CDuration::FromString(this->file->output_freq.getValue());
         this->freq_write     =
             CDuration::FromString(this->file->output_freq.getValue());
         this->last_Write     = boost::shared_ptr<xmlioserver::date::CDate>
                        (new date::CDate(_context->getCalendar()->getInitDate()));
         this->last_operation = boost::shared_ptr<xmlioserver::date::CDate>
                        (new date::CDate(_context->getCalendar()->getInitDate()));
         this->foperation     =
             boost::shared_ptr<func::CFunctor>(new CInstant(this->data));
             
         const CDuration toffset = this->freq_operation - freq_offset_ - _context->getCalendar()->getTimeStep(); 
         *this->last_operation   = *this->last_operation - toffset; 
      }
      else
      {                  
         this->freq_operation = CDuration::FromString(freq_op.getValue());
         this->freq_write     = CDuration::FromString(this->file->output_freq.getValue());
         this->last_Write     = boost::shared_ptr<xmlioserver::date::CDate>
                        (new date::CDate(_context->getCalendar()->getInitDate()));
         this->last_operation = boost::shared_ptr<xmlioserver::date::CDate>
                        (new date::CDate(_context->getCalendar()->getInitDate()));
                        
         const CDuration toffset = this->freq_operation - freq_offset_ - _context->getCalendar()->getTimeStep(); 
         *this->last_operation   = *this->last_operation - toffset;  
         
#define DECLARE_FUNCTOR(MType, mtype)              \
   if  (operation.getValue().compare(#mtype) == 0) \
   {                                               \
      boost::shared_ptr<func::CFunctor>            \
            foperation_(new C##MType(this->data)); \
      this->foperation = foperation_;              \
      return;                                      \
   }
   
#include "functor_type.conf"
         
         ERROR("CField::solveOperation(void)",
               << "[ operation = " << operation.getValue() << "]"
               << "L'opération n'est pas définie dans le code !");
      }
   }
   
   //----------------------------------------------------------------
   
   void CField::fromBinary(StdIStream & is)
   {
      SuperClass::fromBinary(is);
#define CLEAR_ATT(name_)\
      SuperClassAttribute::operator[](#name_)->clear()

         CLEAR_ATT(domain_ref);
         CLEAR_ATT(axis_ref);
#undef CLEAR_ATT

   }

   //----------------------------------------------------------------

   void CField::solveGridReference(void)
   {
      boost::shared_ptr<CDomain> domain;
      boost::shared_ptr<CAxis> axis;

      if (!domain_ref.isEmpty())
      {
         if (CObjectFactory::HasObject<CDomain>(domain_ref.getValue()))
            domain = CObjectFactory::GetObject<CDomain>(domain_ref.getValue()) ;
         else
            ERROR("CField::solveGridReference(void)",
                  << "Référence au domaine nommé \'"
                  << domain_ref.getValue() << "\' incorrecte") ;
      }

      if (!axis_ref.isEmpty())
      {
         if (CObjectFactory::HasObject<CAxis>(axis_ref.getValue()))
            axis = CObjectFactory::GetObject<CAxis>(axis_ref.getValue()) ;
         else
            ERROR("CField::solveGridReference(void)",
                  << "Référence à l'axe nommé \'"
                  << axis_ref.getValue() <<"\' incorrecte") ;
      }

      if (!grid_ref.isEmpty())
      {
         if (CObjectFactory::HasObject<CGrid>(grid_ref.getValue()))
            this->grid = CObjectFactory::GetObject<CGrid>(grid_ref.getValue()) ;
         else
            ERROR("CField::solveGridReference(void)",
                  << "Référence à la grille nommée \'"
                  << grid_ref.getValue() << "\' incorrecte");
         if (!domain_ref.isEmpty())
            DEBUG(<< "Définition conjointe de la grille "
                  << "et du domaine, la grille prévaut..." );
         if (!axis_ref.isEmpty())
            DEBUG(<< "Définition conjointe de la grille "
                  << "et de l'axe vertical, la grille prévaut...") ;
      }
      else
      {
         if (!domain_ref.isEmpty())
         {
            if (!axis_ref.isEmpty())
            {
               this->grid = CGrid::CreateGrid(domain, axis) ;
               this->grid_ref.setValue(this->grid->getId());
            }
            else
            {
               this->grid = CGrid::CreateGrid(domain) ;
               this->grid_ref.setValue(this->grid->getId());
            }
         }
         else
         {
            ERROR("CField::solveGridReference(void)",
                  << "Le domaine horizontal pour le champ X n'est pas défini");
         }
      }
      grid->solveReference() ;
   }

   } // namespace tree

   ///-------------------------------------------------------------------

   template <>
      void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)
   {
      if (this->group_ref.isEmpty()) return;
      StdString gref = this->group_ref.getValue();

      if (!CObjectFactory::HasObject<CFieldGroup>(gref))
         ERROR("CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)",
               << "[ gref = " << gref << "]"
               << " invalid group name !");

      boost::shared_ptr<CFieldGroup> group
               = CObjectFactory::GetObject<CFieldGroup>(gref);
      boost::shared_ptr<CFieldGroup> owner
               = CObjectFactory::GetObject<CFieldGroup>
                  (boost::polymorphic_downcast<CFieldGroup*>(this));

      std::vector<boost::shared_ptr<CField> > allChildren  = group->getAllChildren();
      std::vector<boost::shared_ptr<CField> >::iterator 
         it = allChildren.begin(), end = allChildren.end();
      
      for (; it != end; it++)
      {
         boost::shared_ptr<CField> child = *it;
         if (child->hasId())
            CGroupFactory::CreateChild(owner)->field_ref.setValue(child->getId());
      }
   }

   ///-------------------------------------------------------------------

} // namespace xmlioserver
