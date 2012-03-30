#include "field.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "node_type.hpp"
#include "calendar_util.hpp"

namespace xios{
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
   
   bool CField::dispatchEvent(CEventServer& event)
  {
     
    if (SuperClass::dispatchEvent(event)) return true ;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_UPDATE_DATA :
          recvUpdateData(event) ;
          return true ;
          break ;
 
        default :
          ERROR("bool CField::dispatchEvent(CEventServer& event)",<<"Unknown Event") ;
          return false ;
      }
    }
  }
  
  void CField::sendUpdateData(void)
  {
    shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
    CContextClient* client=context->client ;
    
    CEventClient event(getType(),EVENT_ID_UPDATE_DATA) ;
    
    map<int,ARRAY(int, 1)>::iterator it ;
    list<shared_ptr<CMessage> > list_msg ;
    list<ARRAY(double,1) > list_data ;
    
    for(it=grid->storeIndex_toSrv.begin();it!=grid->storeIndex_toSrv.end();it++)
    {
      int rank=(*it).first ;
      ARRAY(int,1) index=(*it).second ;
      ARRAY_CREATE(data_tmp,double,1,[index->num_elements()]) ;
      for(int n=0;n<data_tmp->num_elements();n++) (*data_tmp)[n]=(*data)[(*index)[n]] ;
      list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
      list_data.push_back(data_tmp) ;
      *list_msg.back()<<getId()<<list_data.back() ;
      event.push(rank,grid->nbSenders[rank],*list_msg.back()) ;
    }
    client->sendEvent(event) ;
  }
  
  void CField::recvUpdateData(CEventServer& event)
  {
    vector<int> ranks ;
    vector<CBufferIn*> buffers ;
      
    list<CEventServer::SSubEvent>::iterator it ;
    string fieldId ;

    for (it=event.subEvents.begin();it!=event.subEvents.end();++it)
    {
      int rank=it->rank;
      CBufferIn* buffer=it->buffer;
      *buffer>>fieldId ;
      ranks.push_back(rank) ;
      buffers.push_back(buffer) ;
    }
    get(fieldId)->recvUpdateData(ranks,buffers) ;   
  }
  
  void  CField::recvUpdateData(vector<int>& ranks, vector<CBufferIn*>& buffers)
  {
    
    if (data_srv.empty())
    {
      for(map<int,ARRAY(int, 1)>::iterator it=grid->out_i_fromClient.begin();it!=grid->out_i_fromClient.end();it++)
      {
        int rank=it->first ;
        ARRAY_CREATE(data_tmp,double,1,[it->second->num_elements()]) ;
        data_srv.insert(pair<int, ARRAY(double,1)>(rank,data_tmp)) ;
        foperation_srv.insert(pair<int,boost::shared_ptr<func::CFunctor> >(rank,boost::shared_ptr<func::CFunctor>(new func::CInstant(data_srv[rank])))) ;
      }
    }

    shared_ptr<CContext> context=CObjectFactory::GetObject<CContext>(CObjectFactory::GetCurrentContextId()) ;
    const date::CDate & currDate = context->getCalendar()->getCurrentDate();
    const date::CDate opeDate      = *last_operation_srv + freq_operation_srv;
    const date::CDate writeDate    = *last_Write_srv     + freq_write_srv; 
    

    
    if (opeDate <= currDate)
    {
      for(int n=0;n<ranks.size();n++)
      {
        ARRAY_CREATE(data_tmp,double,1,[0]) ;
        *buffers[n]>>data_tmp ;
        (*foperation_srv[ranks[n]])(data_tmp) ;
      }
      *last_operation_srv = currDate;
    }
     
    if (writeDate < (currDate + freq_operation_srv))
    {
      for(int n=0;n<ranks.size();n++)
      {
        this->foperation_srv[ranks[n]]->final();
      }
      
      *last_Write_srv = writeDate;
      writeField() ;
      *lastlast_Write_srv=*last_Write_srv;
    }
  }
  
  void CField::writeField(void)
  {
    if (! grid->domain->isEmpty() || getRelFile()->type.getValue()=="one_file")
    {
      getRelFile()->checkFile();
      this->incrementNStep();
      getRelFile()->getDataOutput()->writeFieldData(CObjectFactory::GetObject<CField>(this));
    }
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
 
   void CField::resetNStep(void)
   {
      this->nstep=0;
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
   
   bool CField::isActive(void) const
   { 
      return (!this->refObject.empty()); 
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
      boost::shared_ptr<CContext> context =
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

//      if (CXIOSManager::GetStatus() == CXIOSManager::LOC_SERVER)
      if (context->hasServer)
      {
         this->freq_operation_srv =
             CDuration::FromString(this->file->output_freq.getValue());
         this->freq_write_srv     =
             CDuration::FromString(this->file->output_freq.getValue());
         this->lastlast_Write_srv     = boost::shared_ptr<xios::date::CDate>
                        (new date::CDate(context->getCalendar()->getInitDate()));
         this->last_Write_srv     = boost::shared_ptr<xios::date::CDate>
                        (new date::CDate(context->getCalendar()->getInitDate()));
         this->last_operation_srv = boost::shared_ptr<xios::date::CDate>
                        (new date::CDate(context->getCalendar()->getInitDate()));
//         this->foperation_srv     =
//             boost::shared_ptr<func::CFunctor>(new CInstant(this->data_srv));
             
         const CDuration toffset = this->freq_operation_srv - freq_offset_ - context->getCalendar()->getTimeStep(); 
         *this->last_operation_srv   = *this->last_operation_srv - toffset; 
      }
      
      if (context->hasClient)
      {                  
         this->freq_operation = CDuration::FromString(freq_op.getValue());
         this->freq_write     = CDuration::FromString(this->file->output_freq.getValue());
         this->last_Write     = boost::shared_ptr<xios::date::CDate>
                        (new date::CDate(context->getCalendar()->getInitDate()));
         this->last_operation = boost::shared_ptr<xios::date::CDate>
                        (new date::CDate(context->getCalendar()->getInitDate()));
                        
         const CDuration toffset = this->freq_operation - freq_offset_ - context->getCalendar()->getTimeStep(); 
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
   
   void CField::outputField(ARRAY(double,3) fieldOut)
   {
      map<int,ARRAY(double,1)>::iterator it;
      for(it=data_srv.begin();it!=data_srv.end();it++)
         grid->outputField(it->first,it->second, fieldOut) ;
      
   }
   
   void CField::outputField(ARRAY(double,2) fieldOut)
   {
      map<int,ARRAY(double,1)>::iterator it;

      for(it=data_srv.begin();it!=data_srv.end();it++)
      {
         grid->outputField(it->first,it->second, fieldOut) ;
      }
   }
   ///-------------------------------------------------------------------

} // namespace xios
