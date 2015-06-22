#include "field.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "node_type.hpp"
#include "calendar_util.hpp"
#include "message.hpp"
#include "xios_spl.hpp"
#include "type.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include <set>

namespace xios{

   /// ////////////////////// Définitions ////////////////////// ///

   CField::CField(void)
      : CObjectTemplate<CField>(), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , freq_operation(), freq_write()
      , nstep(0), nstepMax(0), isEOF(false)
      , last_Write(), last_operation()
      , foperation(), hasInstantData(false), hasExpression(false)
      , active(false) , hasOutputFile(false),hasFieldOut(false), slotUpdateDate(NULL)
      , processed(false), domAxisIds_("", ""), areAllReferenceSolved(false), areAllExpressionBuilt(false)
      , isReadDataRequestPending(false)
      , filterSources_()
      { setVirtualVariableGroup(); }

   CField::CField(const StdString& id)
      : CObjectTemplate<CField>(id), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , freq_operation(), freq_write()
      , nstep(0), nstepMax(0), isEOF(false)
      , last_Write(), last_operation()
      , foperation(), hasInstantData(false), hasExpression(false)
      , active(false), hasOutputFile(false), hasFieldOut(false), slotUpdateDate(NULL)
      , processed(false), domAxisIds_("", ""), areAllReferenceSolved(false), areAllExpressionBuilt(false)
      , isReadDataRequestPending(false)
      , filterSources_()
   { setVirtualVariableGroup(); }

   CField::~CField(void)
   {
//      this->grid.reset();
//      this->file.reset();
      this->foperation.reset();
      if (hasExpression) delete expression;
      if (slotUpdateDate != NULL) delete slotUpdateDate;

   }


  //----------------------------------------------------------------

   void CField::setVirtualVariableGroup(CVariableGroup* newVVariableGroup)
   {
      this->vVariableGroup = newVVariableGroup;
   }

   void CField::setVirtualVariableGroup(void)
   {
      this->setVirtualVariableGroup(CVariableGroup::create());
   }

   CVariableGroup* CField::getVirtualVariableGroup(void) const
   {
      return this->vVariableGroup;
   }


   std::vector<CVariable*> CField::getAllVariables(void) const
   {
      return this->vVariableGroup->getAllChildren();
   }

   void CField::solveDescInheritance(bool apply, const CAttributeMap* const parent)
   {
      SuperClassAttribute::setAttributes(parent, apply);
      this->getVirtualVariableGroup()->solveDescInheritance(apply, NULL);
   }
   //----------------------------------------------------------------
   //----------------------------------------------------------------

   bool CField::updateDataServer
      (const CDate& currDate,
       const std::deque< CArray<double, 1>* > storedClient)
   {
      const CDate opeDate      = *last_operation + freq_operation;
      const CDate writeDate    = *last_Write     + freq_write;

      if (opeDate <= currDate)
      {
         if (this->data.numElements() != this->grid->storeIndex[0]->numElements())
         {
            this->data.resize(this->grid->storeIndex[0]->numElements());
         }
         CArray<double,1> input(data.numElements());
         this->grid->inputFieldServer(storedClient, input);
         (*this->foperation)(input);
         *last_operation = currDate;
      }
      if (writeDate < (currDate + freq_operation))
      {
         this->foperation->final();
         this->incrementNStep();
         *last_Write = writeDate;
         return true;
      }
      return false;
   }

  bool CField::dispatchEvent(CEventServer& event)
  {
    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_UPDATE_DATA :
          recvUpdateData(event);
          return true;
          break;

        case EVENT_ID_READ_DATA :
          recvReadDataRequest(event);
          return true;
          break;

        case EVENT_ID_READ_DATA_READY :
          recvReadDataReady(event);
          return true;
          break;

        case EVENT_ID_ADD_VARIABLE :
          recvAddVariable(event);
          return true;
          break;

        case EVENT_ID_ADD_VARIABLE_GROUP :
          recvAddVariableGroup(event);
          return true;
          break;

        default :
          ERROR("bool CField::dispatchEvent(CEventServer& event)", << "Unknown Event");
          return false;
      }
    }
  }

  void CField::sendUpdateData(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(),EVENT_ID_UPDATE_DATA);

    map<int,CArray<int, 1>* >::iterator it;
    list<shared_ptr<CMessage> > list_msg;
    list< CArray<double,1>* > list_data;

    if (!grid->doGridHaveDataDistributed())
    {
       if (0 == client->clientRank)
       {
          for(it=grid->storeIndex_toSrv.begin();it!=grid->storeIndex_toSrv.end();it++)
          {
            int rank=(*it).first ;
            CArray<int,1>& index = *(it->second) ;
            CArray<double,1> data_tmp(index.numElements()) ;
            for(int n=0;n<data_tmp.numElements();n++) data_tmp(n)=data(index(n)) ;

            list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
            list_data.push_back(new CArray<double,1>(data_tmp)) ;
            *list_msg.back()<<getId()<<*list_data.back() ;
            event.push(rank,1,*list_msg.back()) ;
          }
          client->sendEvent(event) ;
       } else client->sendEvent(event);
    }
    else
    {
      for(it=grid->storeIndex_toSrv.begin();it!=grid->storeIndex_toSrv.end();it++)
      {
        int rank=(*it).first ;
        CArray<int,1>& index = *(it->second) ;
        CArray<double,1> data_tmp(index.numElements()) ;
        for(int n=0;n<data_tmp.numElements();n++) data_tmp(n)=data(index(n)) ;
        list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
        list_data.push_back(new CArray<double,1>(data_tmp)) ;
        *list_msg.back()<<getId()<<*list_data.back() ;
        event.push(rank,grid->nbSenders[rank],*list_msg.back()) ;
      }
      client->sendEvent(event) ;
    }

    for (list< CArray<double,1>* >::iterator it = list_data.begin(); it != list_data.end(); it++) delete *it;
  }

  void CField::recvUpdateData(CEventServer& event)
  {
    vector<int> ranks;
    vector<CBufferIn*> buffers;

    list<CEventServer::SSubEvent>::iterator it;
    string fieldId;

    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      int rank = it->rank;
      CBufferIn* buffer = it->buffer;
      *buffer >> fieldId;
      ranks.push_back(rank);
      buffers.push_back(buffer);
    }
    get(fieldId)->recvUpdateData(ranks,buffers);
  }

  void  CField::recvUpdateData(vector<int>& ranks, vector<CBufferIn*>& buffers)
  {
    if (data_srv.empty())
    {
      for (map<int, CArray<size_t, 1>* >::iterator it = grid->outIndexFromClient.begin(); it != grid->outIndexFromClient.end(); ++it)
      {
        int rank = it->first;
        CArray<double,1> data_tmp(it->second->numElements());
        data_srv.insert( pair<int, CArray<double,1>* >(rank, new CArray<double,1>(data_tmp)));
        foperation_srv.insert(pair<int,boost::shared_ptr<func::CFunctor> >(rank,boost::shared_ptr<func::CFunctor>(new func::CInstant(*data_srv[rank]))));
      }
    }

    CContext* context = CContext::getCurrent();
    const CDate& currDate = context->getCalendar()->getCurrentDate();
    const CDate opeDate      = *last_operation_srv + freq_operation_srv;
    const CDate writeDate    = *last_Write_srv     + freq_write_srv;

    if (opeDate <= currDate)
    {
      for (int n = 0; n < ranks.size(); n++)
      {
        CArray<double,1> data_tmp;
        *buffers[n] >> data_tmp;
        (*foperation_srv[ranks[n]])(data_tmp);
      }
      *last_operation_srv = currDate;
    }

    if (writeDate < (currDate + freq_operation_srv))
    {
      for (int n = 0; n < ranks.size(); n++)
      {
        this->foperation_srv[ranks[n]]->final();
      }

      *last_Write_srv = writeDate;
      writeField();
      *lastlast_Write_srv = *last_Write_srv;
    }
  }

  void CField::writeField(void)
  {
    if (!getRelFile()->allDomainEmpty)
    {
      if (grid->doGridHaveDataToWrite() || getRelFile()->type == CFile::type_attr::one_file)
      {
        getRelFile()->checkFile();
        this->incrementNStep();
        getRelFile()->getDataOutput()->writeFieldData(CField::get(this));
      }
    }
  }

  void CField::sendReadDataRequest(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_READ_DATA);
    if (client->isServerLeader())
    {
      CMessage msg;
      msg << getId();
      const std::list<int>& ranks = client->getRanksServerLeader();
      for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
        event.push(*itRank, 1, msg);
      client->sendEvent(event);
    }
    else client->sendEvent(event);

    lastDataRequestedFromServer = context->getCalendar()->getCurrentDate();
    isReadDataRequestPending = true;
  }

  /*!
  Send request new data read from file if need be, that is the current data is out-of-date.
  \return true if and only if some data was requested
  */
  bool CField::sendReadDataRequestIfNeeded(void)
  {
    const CDate& currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate();

    bool requestData = (currentDate >= lastDataRequestedFromServer + file->output_freq.getValue());

    if (requestData)
      sendReadDataRequest();

    return requestData;
  }

  void CField::recvReadDataRequest(CEventServer& event)
  {
    CBufferIn* buffer = event.subEvents.begin()->buffer;
    StdString fieldId;
    *buffer >> fieldId;
    get(fieldId)->recvReadDataRequest();
  }

  void CField::recvReadDataRequest(void)
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_READ_DATA_READY);
    std::list<CMessage> msgs;

    bool hasData = readField();

    map<int, CArray<double,1>* >::iterator it;
    for (it = data_srv.begin(); it != data_srv.end(); it++)
    {
      msgs.push_back(CMessage());
      CMessage& msg = msgs.back();
      msg << getId();
      if (hasData)
        msg << getNStep() - 1 << *it->second;
      else
        msg << size_t(-1);
      event.push(it->first, grid->nbSenders[it->first], msg);
    }
    client->sendEvent(event);
  }

  bool CField::readField(void)
  {
    if (!getRelFile()->allDomainEmpty)
    {
      if (grid->doGridHaveDataToWrite() || getRelFile()->type == CFile::type_attr::one_file)
      {
        if (data_srv.empty())
        {
          for (map<int, CArray<size_t, 1>* >::iterator it = grid->outIndexFromClient.begin(); it != grid->outIndexFromClient.end(); ++it)
            data_srv.insert(pair<int, CArray<double,1>*>(it->first, new CArray<double,1>(it->second->numElements())));
        }

        getRelFile()->checkFile();
        this->incrementNStep();

        if (!nstepMax)
        {
          nstepMax = getRelFile()->getDataInput()->getFieldNbRecords(CField::get(this));
        }

        if (getNStep() > nstepMax)
          return false;

        getRelFile()->getDataInput()->readFieldData(CField::get(this));
      }
    }

    return true;
  }

  void CField::recvReadDataReady(CEventServer& event)
  {
    string fieldId;
    vector<int> ranks;
    vector<CBufferIn*> buffers;

    list<CEventServer::SSubEvent>::iterator it;
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      ranks.push_back(it->rank);
      CBufferIn* buffer = it->buffer;
      *buffer >> fieldId;
      buffers.push_back(buffer);
    }
    get(fieldId)->recvReadDataReady(ranks, buffers);
  }

  void CField::recvReadDataReady(vector<int> ranks, vector<CBufferIn*> buffers)
  {
    CContext* context = CContext::getCurrent();
    StdSize record;
    for (int i = 0; i < ranks.size(); i++)
    {
      int rank = ranks[i];
      *buffers[i] >> record;
      isEOF = (record == size_t(-1));

      if (!isEOF)
      {
        CArray<int,1>& index = *grid->storeIndex_toSrv[rank];
        CArray<double,1> data_tmp(index.numElements());
        *buffers[i] >> data_tmp;
        for (int n = 0; n < data_tmp.numElements(); n++)
          instantData(index(n)) = data_tmp(n);
      }
      else
        break;
    }

    if (!isEOF)
    {
      for (list< pair<CField*, int> >::iterator it = fieldDependency.begin(); it != fieldDependency.end(); ++it)
        it->first->setSlot(it->second);

      if (!hasExpression) // Should be always true ?
      {
        const std::vector<CField*>& refField = getAllReference();
        std::vector<CField*>::const_iterator it = refField.begin(), end = refField.end();

        for (; it != end; it++) (*it)->setDataFromExpression(instantData);
        if (hasFieldOut) updateDataFromExpression(instantData);
      }
    }

    isReadDataRequestPending = false;
  }

   //----------------------------------------------------------------

   void CField::setRelFile(CFile* _file)
   {
      this->file = _file;
      hasOutputFile = true;
   }

   //----------------------------------------------------------------

   StdString CField::GetName(void)    { return StdString("field"); }
   StdString CField::GetDefName(void) { return CField::GetName(); }
   ENodeType CField::GetType(void)    { return eField; }

   //----------------------------------------------------------------

   CGrid* CField::getRelGrid(void) const
   {
      return this->grid;
   }

   //----------------------------------------------------------------

   CFile* CField::getRelFile(void) const
   {
      return this->file;
   }

   StdSize CField::getNStep(void) const
   {
      return this->nstep;
   }

   void CField::incrementNStep(void)
   {
      this->nstep++;
   }

   void CField::resetNStep(void)
   {
      this->nstep = 0;
   }

   void CField::resetNStepMax(void)
   {
      this->nstepMax = 0;
   }

   //----------------------------------------------------------------

   const CDuration& CField::getFreqOperation(void) const
   {
      return this->freq_operation;
   }

   //----------------------------------------------------------------

   const CDuration& CField::getFreqWrite(void) const
   {
      return this->freq_write;
   }

   //----------------------------------------------------------------

   boost::shared_ptr<func::CFunctor> CField::getFieldOperation(void) const
   {
      return this->foperation;
   }

   bool CField::isActive(void) const
   {
      return !this->refObject.empty();
   }

   //----------------------------------------------------------------

   CArray<double, 1> CField::getData(void) const
   {
      return(this->data);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<CDate> CField::getLastWriteDate(void) const
   {
      return(this->last_Write);
   }

   //----------------------------------------------------------------

   boost::shared_ptr<CDate> CField::getLastOperationDate(void) const
   {
      return(this->last_operation);
   }

   void CField::solveAllReferenceEnabledField(bool doSending2Sever)
   {
     CContext* context = CContext::getCurrent();
     if (!areAllReferenceSolved)
     {
        areAllReferenceSolved = true;
        if (context->hasClient)
        {
          solveRefInheritance(true);
          solveBaseReference();
        }

        solveOperation();
        solveGridReference();
     }
     solveGridDomainAxisRef(doSending2Sever);
     if (context->hasClient)
     {
       solveTransformedGrid();
     }
     solveCheckMaskIndex(doSending2Sever);
   }

   std::map<int, StdSize> CField::getGridDataSize()
   {
     return grid->getConnectedServerDataSize();
   }

   void CField::buildAllExpressionEnabledField()
   {
     if (!areAllReferenceSolved) solveAllReferenceEnabledField(true);
     if (!areAllExpressionBuilt)
     {
       areAllExpressionBuilt = true;
       if (hasDirectFieldReference() && (grid_ref.isEmpty())) baseRefObject->buildAllExpressionEnabledField();
       buildExpression();
       active = true;
     }
   }

   //----------------------------------------------------------------

   void  CField::solveOperation(void)
   {
      using namespace func;

      if (!hasOutputFile && !hasFieldOut) return;

      StdString id;
      if (hasId()) id = getId();
      else if (!name.isEmpty()) id = name;
      else if (hasDirectFieldReference()) id = baseRefObject->getId();

      CContext* context = CContext::getCurrent();

      if (freq_op.isEmpty()) freq_op.setValue(TimeStep);

      if (operation.isEmpty())
      {
         ERROR("CField::solveOperation(void)",
               << "[ id = " << id << "]"
               << "Impossible to define an operation for this field !");
      }

      if (freq_offset.isEmpty())
        freq_offset.setValue(NoneDu);

//      if (CXIOSManager::GetStatus() == CXIOSManager::LOC_SERVER)
      if (context->hasServer)
      {
         if (hasOutputFile)
         {
           this->freq_operation_srv = this->file->output_freq.getValue();
           this->freq_write_srv = this->file->output_freq.getValue();
         }
         this->lastlast_Write_srv = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
         this->last_Write_srv     = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
         this->last_operation_srv = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
//         this->foperation_srv     =
//             boost::shared_ptr<func::CFunctor>(new CInstant(this->data_srv));

         if (hasOutputFile)
         {
           const CDuration toffset = this->freq_operation_srv - freq_offset.getValue() - context->getCalendar()->getTimeStep();
           *this->last_operation_srv   = *this->last_operation_srv - toffset;
         }
      }

//      if (context->hasClient)
//      {
         this->freq_operation = freq_op.getValue();
         if (hasOutputFile) this->freq_write = this->file->output_freq.getValue();
         if (hasFieldOut)
         {
           this->freq_write = this->fieldOut->freq_op.getValue();
         }
         this->last_Write     = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));
         this->last_operation = boost::shared_ptr<CDate>
                        (new CDate(context->getCalendar()->getInitDate()));

         const CDuration toffset = this->freq_operation - freq_offset.getValue() - context->getCalendar()->getTimeStep();
         *this->last_operation   = *this->last_operation - toffset;

         lastDataRequestedFromServer.setRelCalendar(*context->getCalendar());

        if (operation.get() == "once") isOnceOperation = true;
        else isOnceOperation = false;
        isFirstOperation = true;

#define DECLARE_FUNCTOR(MType, mtype) \
   if (operation.getValue().compare(#mtype) == 0) \
   { \
      if (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true) \
      { \
        boost::shared_ptr<func::CFunctor> foperation_(new C##MType(this->data,default_value)); \
        this->foperation = foperation_; \
      } \
      else \
      { \
        boost::shared_ptr<func::CFunctor> foperation_(new C##MType(this->data)); \
        this->foperation = foperation_; \
      } \
      return; \
   }

#include "functor_type.conf"

         ERROR("CField::solveOperation(void)",
               << "[ operation = " << operation.getValue() << "]"
               << "The operation is not defined !");
//      }
   }

   //----------------------------------------------------------------
/*
   void CField::fromBinary(StdIStream& is)
   {
      SuperClass::fromBinary(is);
#define CLEAR_ATT(name_)\
      SuperClassAttribute::operator[](#name_)->reset()

         CLEAR_ATT(domain_ref);
         CLEAR_ATT(axis_ref);
#undef CLEAR_ATT

   }
*/
   //----------------------------------------------------------------

   void CField::solveGridReference(void)
   {
      CDomain* domain;
      CAxis* axis;
      std::vector<CDomain*> vecDom;
      std::vector<CAxis*> vecAxis;
      std::vector<std::string> domList, axisList;

      if (!domain_ref.isEmpty())
      {
         if (CDomain::has(domain_ref.getValue()))
         {
           domain = CDomain::get(domain_ref.getValue());
           vecDom.push_back(domain);
         }
         else
            ERROR("CField::solveGridReference(void)",
                  << "Reference to the domain \'"
                  << domain_ref.getValue() << "\' is wrong");
      }

      if (!axis_ref.isEmpty())
      {
         if (CAxis::has(axis_ref.getValue()))
         {
           axis = CAxis::get(axis_ref.getValue());
           vecAxis.push_back(axis);
         }
         else
            ERROR("CField::solveGridReference(void)",
                  << "Reference to the axis \'"
                  << axis_ref.getValue() <<"\' is wrong");
      }

      if (!grid_ref.isEmpty())
      {
         if (CGrid::has(grid_ref.getValue()))
         {
           this->grid = CGrid::get(grid_ref.getValue());
           domList = grid->getDomainList();
           axisList = grid->getAxisList();
           if (domList.empty() && axisList.empty())
           {
             this->grid = CGrid::createGrid(vecDom, vecAxis);
           }
         }
         else
            ERROR("CField::solveGridReference(void)",
                  << "Reference to the grid \'"
                  << grid_ref.getValue() << "\' is wrong");
      }
      else
      {
         this->grid = CGrid::createGrid(vecDom, vecAxis);
      }

      if (grid_ref.isEmpty() && domain_ref.isEmpty() && axis_ref.isEmpty())
      {
            ERROR("CField::solveGridReference(void)",
                  << "At least one dimension must be defined for this field.");
      }
   }

   void CField::solveGridDomainAxisRef(bool checkAtt)
   {
     grid->solveDomainAxisRef(checkAtt);
   }

   void CField::solveCheckMaskIndex(bool doSendingIndex)
   {
     grid->checkMaskIndex(doSendingIndex);
   }

   CGrid* CField::getGridRefOfBaseReference()
   {
     solveRefInheritance(true);
     solveBaseReference();
     baseRefObject->solveGridReference();

     return baseRefObject->grid;
   }

   void CField::solveTransformedGrid()
   {
     if (!grid_ref.isEmpty() && (!field_ref.isEmpty()))
     {
       CField* fieldRef  = this;
       CGrid* gridRefOfFieldRef = 0;
       while (fieldRef->hasDirectFieldReference())
       {
         if ((!(fieldRef->grid_ref.isEmpty())) &&
             (fieldRef->grid_ref.getValue() != grid_ref.getValue()))
         {
           gridRefOfFieldRef = fieldRef->getGridRefOfBaseReference();
           fieldRef->addReference(this);
           fieldRef->solveGridDomainAxisRef(false);
           break;
         }
         CField* tmp = fieldRef->getDirectFieldReference();
         fieldRef = tmp;
       }

       if ((0 == gridRefOfFieldRef) &&
           (!(fieldRef->grid_ref.isEmpty())) &&
           (fieldRef->grid_ref.getValue() != grid_ref.getValue()))
       {
         gridRefOfFieldRef = fieldRef->getGridRefOfBaseReference();
         fieldRef->addReference(this);
         fieldRef->solveGridDomainAxisRef(false);
       }

       CGrid* relGridRef = CGrid::get(grid_ref.getValue());
       if ((0 != gridRefOfFieldRef) && (relGridRef != gridRefOfFieldRef) && (!(relGridRef->isTransformed())))
       {
         gridRefOfFieldRef->transformGrid(relGridRef);
         filterSources_.push_back(fieldRef);
       }
     }
   }

   const std::vector<CField*>& CField::getFilterSources()
   {
     return filterSources_;
   }

   void CField::applyFilter(const CArray<double, 1>& dataToSend, CArray<double,1>& dataToReceive)
   {
     std::vector<CField*>::iterator  itFilterSrc, iteFilterSrc;
     if (!filterSources_.empty())
     {
        itFilterSrc = filterSources_.begin(); iteFilterSrc = filterSources_.end();
        for (; itFilterSrc != iteFilterSrc; ++itFilterSrc)
        {
          if (0 != (*itFilterSrc)->grid->getTransformations())
          {
             const std::map<int, CArray<int,1>* >& localIndexToSend = (*itFilterSrc)->grid->getTransformations()->getLocalIndexToSendFromGridSource();
             const std::map<int, std::vector<CArray<int,1>* > >& localIndexToReceive = (*itFilterSrc)->grid->getTransformations()->getLocalIndexToReceiveOnGridDest();

             sendAndReceiveTransformedData(localIndexToSend, dataToSend,
                                           localIndexToReceive, dataToReceive);
          }

        }
     }
   }

   void CField::sendAndReceiveTransformedData(const std::map<int, CArray<int,1>* >& localIndexToSend,
                                              const CArray<double, 1>& dataSrc,
                                              const std::map<int, std::vector<CArray<int,1>* > >& localIndexToReceive,
                                              CArray<double,1>& dataDest)
   {
     CContext* context = CContext::getCurrent();
     CContextClient* client=context->client;

     // Sending data from field sources to do transformations
     std::map<int, CArray<int,1>* >::const_iterator itbSend = localIndexToSend.begin(), itSend,
                                                    iteSend = localIndexToSend.end();
     int sendBuffSize = 0;
     for (itSend = itbSend; itSend != iteSend; ++itSend) sendBuffSize = (sendBuffSize < (itSend->second)->numElements())
                                                                        ? (itSend->second)->numElements(): sendBuffSize;
     double* sendBuff;
     if (0 != sendBuffSize) sendBuff = new double [sendBuffSize];
     for (itSend = itbSend; itSend != iteSend; ++itSend)
     {
       int destRank = itSend->first;
       CArray<int,1>* localIndex_p = itSend->second;
       int countSize = localIndex_p->numElements();
       for (int idx = 0; idx < countSize; ++idx)
       {
         sendBuff[idx] = dataSrc((*localIndex_p)(idx));
       }
       MPI_Send(sendBuff, countSize, MPI_DOUBLE, destRank, 12, client->intraComm);
     }

     // Receiving data on destination fields
     std::map<int, std::vector<CArray<int,1>* > >::const_iterator itbRecv = localIndexToReceive.begin(), itRecv,
                                                                  iteRecv = localIndexToReceive.end();
     int recvBuffSize = 0;
     for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv) recvBuffSize = (recvBuffSize < (itRecv->second).size())
                                                                        ? (itRecv->second).size() : recvBuffSize;
     double* recvBuff;
     if (0 != recvBuffSize) recvBuff = new double [recvBuffSize];
     for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
     {
       MPI_Status status;
       int srcRank = itRecv->first;
       int countSize = (itRecv->second).size();
       MPI_Recv(recvBuff, recvBuffSize, MPI_DOUBLE, srcRank, 12, client->intraComm, &status);
       for (int idx = 0; idx < countSize; ++idx)
       {
         CArray<int,1>* localIndex_p = (itRecv->second)[idx];
         int numIndex = localIndex_p->numElements();
         for (int i = 0; i < numIndex; ++i)
         {
           dataDest((*localIndex_p)(i)) = recvBuff[idx];
         }
       }
     }

     if (0 != sendBuffSize) delete [] sendBuff;
     if (0 != recvBuffSize) delete [] recvBuff;
   }

   ///-------------------------------------------------------------------

   template <>
   void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)
   {
      if (this->group_ref.isEmpty()) return;
      StdString gref = this->group_ref.getValue();

      if (!CFieldGroup::has(gref))
         ERROR("CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)",
               << "[ gref = " << gref << "]"
               << " invalid group name !");

      CFieldGroup* group = CFieldGroup::get(gref);
      CFieldGroup* owner = CFieldGroup::get(boost::polymorphic_downcast<CFieldGroup*>(this));

      std::vector<CField*> allChildren  = group->getAllChildren();
      std::vector<CField*>::iterator it = allChildren.begin(), end = allChildren.end();

      for (; it != end; it++)
      {
         CField* child = *it;
         if (child->hasId()) owner->createChild()->field_ref.setValue(child->getId());

      }
   }

   void CField::scaleFactorAddOffset(double scaleFactor, double addOffset)
   {
     map<int, CArray<double,1>* >::iterator it;
     for (it = data_srv.begin(); it != data_srv.end(); it++) *it->second = (*it->second - addOffset) / scaleFactor;
   }

   void CField::invertScaleFactorAddOffset(double scaleFactor, double addOffset)
   {
     map<int, CArray<double,1>* >::iterator it;
     for (it = data_srv.begin(); it != data_srv.end(); it++) *it->second = *it->second * scaleFactor + addOffset;
   }

   void CField::outputField(CArray<double,3>& fieldOut)
   {
      map<int, CArray<double,1>* >::iterator it;
      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
        grid->outputField(it->first,*it->second, fieldOut.dataFirst());
      }
   }

   void CField::outputField(CArray<double,2>& fieldOut)
   {
      map<int, CArray<double,1>* >::iterator it;
      for(it=data_srv.begin();it!=data_srv.end();it++)
      {
         grid->outputField(it->first,*it->second, fieldOut.dataFirst()) ;
      }
   }

   void CField::outputField(CArray<double,1>& fieldOut)
   {
      map<int, CArray<double,1>* >::iterator it;

      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->outputField(it->first,*it->second, fieldOut.dataFirst()) ;
      }
   }

   void CField::inputField(CArray<double,3>& fieldOut)
   {
      map<int, CArray<double,1>*>::iterator it;
      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
        grid->inputField(it->first, fieldOut.dataFirst(), *it->second);
      }
   }

   void CField::inputField(CArray<double,2>& fieldOut)
   {
      map<int, CArray<double,1>*>::iterator it;
      for(it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->inputField(it->first, fieldOut.dataFirst(), *it->second);
      }
   }

   void CField::inputField(CArray<double,1>& fieldOut)
   {
      map<int, CArray<double,1>*>::iterator it;
      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->inputField(it->first, fieldOut.dataFirst(), *it->second);
      }
   }

   ///-------------------------------------------------------------------

   void CField::parse(xml::CXMLNode& node)
   {
      SuperClass::parse(node);
      if (!node.getContent(this->content))
      {
        if (node.goToChildElement())
        {
          do
          {
            if (node.getElementName() == "variable" || node.getElementName() == "variable_group") this->getVirtualVariableGroup()->parseChild(node);
          } while (node.goToNextElement());
          node.goToParentElement();
        }
      }
    }

  CArray<double,1>* CField::getInstantData(void)
  {
    if (!hasInstantData)
    {
      instantData.resize(grid->storeIndex_client.numElements());
      hasInstantData = true;
    }
    return &instantData;
  }

  void CField::addDependency(CField* field, int slotId)
  {
    fieldDependency.push_back(pair<CField*,int>(field,slotId));
  }

  void CField::buildExpression(void)
  {
    if (content.size() > 0)
    {
      CSimpleNodeExpr* simpleExpr = parseExpr(content+'\0');
      expression = CFieldNode::newNode(simpleExpr);
      delete simpleExpr;
      set<string> instantFieldIds;
      map<string,CField*> associatedInstantFieldIds;
      expression->getInstantFieldIds(instantFieldIds);
      for (set<string>::iterator it = instantFieldIds.begin(); it != instantFieldIds.end(); ++it)
      {
        if (*it != "this")
        {
          if (CField::has(*it))
          {
            CField* field = CField::get(*it);
//            field->processEnabledField();
            field->buildAllExpressionEnabledField();
            associatedInstantFieldIds[*it] = field;
          }
          else  ERROR("void CField::buildExpression(void)", << " Field " << *it << " does not exist");
        }
      }

      set<string> averageFieldIds;
      map<string,CField*> associatedAverageFieldIds;

      expression->getAverageFieldIds(averageFieldIds);
      for (set<string>::iterator it = averageFieldIds.begin(); it != averageFieldIds.end(); ++it)
      {
        if (CField::has(*it))
        {
           CFieldGroup* root = CFieldGroup::get("field_definition");
           CField* averageField = root->createChild();
           CField* instantField = root->createChild();
           averageField->field_ref = *it;
           averageField->hasFieldOut = true;
           averageField->fieldOut = instantField;
           instantField->freq_op = freq_op;
//           averageField-> processEnabledField();
           averageField->buildAllExpressionEnabledField();
           instantField->SuperClassAttribute::setAttributes(averageField, true);
           instantField->field_ref.reset();
           instantField->operation.reset();

//           instantField-> processEnabledField();
           instantField->buildAllExpressionEnabledField();
           associatedAverageFieldIds[*it] = instantField;
        }
        else ERROR("void CField::buildExpression(void)", << " Field " << *it << " does not exist");
      }

      expression->reduce(this,associatedInstantFieldIds,associatedAverageFieldIds);

      slots.resize(instantFieldIds.size() + averageFieldIds.size());
      resetSlots();
      int slotId = 0;
      set<CField*> fields;
      expression->getFields(fields);
      for (set<CField*>::iterator it = fields.begin(); it != fields.end(); ++it, ++slotId) (*it)->addDependency(this,slotId);
      hasExpression = true;
    }
  }

  void CField::resetSlots(void)
  {
    for (vector<bool>::iterator it = slots.begin(); it != slots.end(); ++it) *it = false;
  }

  bool CField::slotsFull(void)
  {
    bool ret = true;
    for (vector<bool>::iterator it = slots.begin(); it != slots.end(); ++it) ret &= *it;
    return ret;
  }

  void CField::setSlot(int slotId)
  {
    CContext* context = CContext::getCurrent();
    const CDate& currDate = context->getCalendar()->getCurrentDate();
    if (slotUpdateDate == NULL || currDate != *slotUpdateDate)
    {
      resetSlots();
      if (slotUpdateDate == NULL) slotUpdateDate = new CDate(currDate);
      else *slotUpdateDate = currDate;
    }
    slots[slotId] = true;
    if (slotsFull())
    {
      CArray<double,1> expr(expression->compute());

      if (hasInstantData)
      {
        instantData = expr;
        for (list< pair<CField *,int> >::iterator it = fieldDependency.begin(); it != fieldDependency.end(); ++it)
          if (it->first != this) it->first->setSlot(it->second);
      }

      if (hasOutputFile) updateDataFromExpression(expr);

      const std::vector<CField*>& refField = getAllReference();
      for (std::vector<CField*>::const_iterator it = refField.begin(); it != refField.end(); it++)
      {
        if (!(*it)->hasExpression)
          (*it)->setDataFromExpression(expr);
      }
    }
  }

   /*!
     This function retrieves Id of corresponding domain_ref and axis_ref (if any)
   of a field. In some cases, only domain exists but axis doesn't
   \return pair of Domain and Axis id
   */
   const std::pair<StdString,StdString>& CField::getRefDomainAxisIds()
   {
     CGrid* cgPtr = getRelGrid();
     if (NULL != cgPtr)
     {
       std::vector<StdString>::iterator it;
       if (!domain_ref.isEmpty())
       {
         std::vector<StdString> domainList = cgPtr->getDomainList();
         it = std::find(domainList.begin(), domainList.end(), domain_ref.getValue());
         if (domainList.end() != it) domAxisIds_.first = *it;
       }

       if (!axis_ref.isEmpty())
       {
         std::vector<StdString> axisList = cgPtr->getAxisList();
         it = std::find(axisList.begin(), axisList.end(), axis_ref.getValue());
         if (axisList.end() != it) domAxisIds_.second = *it;
       }
     }
     return (domAxisIds_);
   }

   CVariable* CField::addVariable(const string& id)
   {
     return vVariableGroup->createChild(id);
   }

   CVariableGroup* CField::addVariableGroup(const string& id)
   {
     return vVariableGroup->createChildGroup(id);
   }

   void CField::sendAddAllVariables()
   {
     if (!getAllVariables().empty())
     {
       // Firstly, it's necessary to add virtual variable group
       sendAddVariableGroup(getVirtualVariableGroup()->getId());

       // Okie, now we can add to this variable group
       std::vector<CVariable*> allVar = getAllVariables();
       std::vector<CVariable*>::const_iterator it = allVar.begin();
       std::vector<CVariable*>::const_iterator itE = allVar.end();

       for (; it != itE; ++it)
       {
         this->sendAddVariable((*it)->getId());
         (*it)->sendAllAttributesToServer();
         (*it)->sendValue();
       }
     }
   }

   void CField::sendAddVariable(const string& id)
   {
    CContext* context = CContext::getCurrent();

    if (!context->hasServer)
    {
       CContextClient* client = context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_VARIABLE);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg << this->getId();
         msg << id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   void CField::sendAddVariableGroup(const string& id)
   {
    CContext* context = CContext::getCurrent();
    if (!context->hasServer)
    {
       CContextClient* client = context->client;

       CEventClient event(this->getType(),EVENT_ID_ADD_VARIABLE_GROUP);
       if (client->isServerLeader())
       {
         CMessage msg;
         msg << this->getId();
         msg << id;
         const std::list<int>& ranks = client->getRanksServerLeader();
         for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
           event.push(*itRank,1,msg);
         client->sendEvent(event);
       }
       else client->sendEvent(event);
    }
   }

   void CField::recvAddVariable(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddVariable(*buffer);
   }

   void CField::recvAddVariable(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addVariable(id);
   }

   void CField::recvAddVariableGroup(CEventServer& event)
   {

      CBufferIn* buffer = event.subEvents.begin()->buffer;
      string id;
      *buffer >> id;
      get(id)->recvAddVariableGroup(*buffer);
   }

   void CField::recvAddVariableGroup(CBufferIn& buffer)
   {
      string id;
      buffer >> id;
      addVariableGroup(id);
   }

   DEFINE_REF_FUNC(Field,field)

//  void CField::addReference(CField* field)
//  {
//    refObject.push_back(field);
//  }
//
//   //----------------------------------------------------------------
//
//   bool CField::hasDirectFieldReference(void) const
//   {
//     return !this->field_ref.isEmpty();
//   }
//
//   //----------------------------------------------------------------
//
//   const StdString& CField::getBaseFieldId(void) const
//   {
//      return this->getBaseFieldReference()->getId();
//   }
//
//   //----------------------------------------------------------------
//
//   /*!
//   \brief Get pointer to direct field to which the current field refers.
//   */
//   CField* CField::getDirectFieldReference(void) const
//   {
//      if (this->field_ref.isEmpty())
//         return this->getBaseFieldReference();
//
//      if (!CField::has(this->field_ref.getValue()))
//         ERROR("CField::getDirectFieldReference(void)",
//               << "[ ref_name = " << this->field_ref.getValue() << "]"
//               << " invalid field name !");
//
//      return CField::get(this->field_ref.getValue());
//   }
//
//   //----------------------------------------------------------------
//
//   CField* CField::getBaseFieldReference(void) const
//   {
//      return baseRefObject;
//   }
//
//   //----------------------------------------------------------------
//
//   const std::vector<CField*>& CField::getAllReference(void) const
//   {
//      return refObject;
//   }
//
//   /*!
//   \brief Searching for all reference of a field
//   If a field refers to (an)other field(s), we will search for all its referenced parents.
//   Moreover, if any father, direct or indirect (e.g: two levels up), has non-empty attributes,
//   all its attributes will be added to the current field
//   \param [in] apply Flag to specify whether current field uses attributes of its father
//               in case the attribute is empty (true) or its attributes are replaced by ones of its father (false)
//   */
//   void CField::solveRefInheritance(bool apply)
//   {
//      std::set<CField *> sset;
//      CField* refer_sptr;
//      CField* refer_ptr = this;
//
//      while (refer_ptr->hasDirectFieldReference())
//      {
//         refer_sptr = refer_ptr->getDirectFieldReference();
//         refer_ptr  = refer_sptr;
//
//         if(sset.end() != sset.find(refer_ptr))
//         {
//            DEBUG (<< "Circular dependency stopped for field object on "
//                   << "\"" + refer_ptr->getId() + "\" !");
//            break;
//         }
//
//         SuperClassAttribute::setAttributes(refer_ptr, apply);
//         sset.insert(refer_ptr);
//      }
//   }
//
//   /*!
//   \brief Only on SERVER side. Remove all field_ref from current field
//   On creating a new field on server side, redundant "field_ref" is still kept in the attribute list
//   of the current field. This function removes this from current field
//   */
//   void CField::removeRefInheritance()
//   {
//     if (this->field_ref.isEmpty()) return;
//     this->clearAttribute("field_ref");
//   }
//
//   void CField::solveBaseReference(void)
//   {
//      std::set<CField *> sset;
//      CField* refer_sptr;
//      CField* refer_ptr = this;
//
//      if (this->hasDirectFieldReference())  baseRefObject = getDirectFieldReference();
//      else  baseRefObject = CField::get(this);
//
//      while (refer_ptr->hasDirectFieldReference())
//      {
//         refer_sptr = refer_ptr->getDirectFieldReference();
//         refer_ptr  = refer_sptr;
//
//         if(sset.end() != sset.find(refer_ptr))
//         {
//            DEBUG (<< "Circular dependency stopped for field object on "
//                   << "\"" + refer_ptr->getId() + "\" !");
//            break;
//         }
//
//         sset.insert(refer_ptr);
//      }
//
//      if (hasDirectFieldReference()) baseRefObject->addReference(this);
//   }
} // namespace xios
