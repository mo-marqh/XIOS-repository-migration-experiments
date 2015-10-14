#include "field.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "node_type.hpp"
#include "calendar_util.hpp"
#include "message.hpp"
#include "xios_spl.hpp"
#include "type.hpp"
#include "timer.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include <set>
#include "garbage_collector.hpp"
#include "source_filter.hpp"
#include "store_filter.hpp"
#include "file_writer_filter.hpp"
#include "pass_through_filter.hpp"
#include "filter_expr_node.hpp"
#include "lex_parser.hpp"
#include "temporal_filter.hpp"
#include "spatial_transform_filter.hpp"

namespace xios{

   /// ////////////////////// Définitions ////////////////////// ///

   CField::CField(void)
      : CObjectTemplate<CField>(), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , written(false)
      , nstep(0), nstepMax(0)
      , hasOutputFile(false)
      , domAxisIds_("", ""), areAllReferenceSolved(false)
      , useCompressedOutput(false)
      , isReadDataRequestPending(false)
   { setVirtualVariableGroup(); }

   CField::CField(const StdString& id)
      : CObjectTemplate<CField>(id), CFieldAttributes()
      , refObject(), baseRefObject()
      , grid(), file()
      , written(false)
      , nstep(0), nstepMax(0)
      , hasOutputFile(false)
      , domAxisIds_("", ""), areAllReferenceSolved(false)
      , useCompressedOutput(false)
      , isReadDataRequestPending(false)
   { setVirtualVariableGroup(); }

   CField::~CField(void)
   {}

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

  void CField::sendUpdateData(const CArray<double,1>& data)
  {
    CTimer::get("XIOS Send Data").resume();

    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;

    CEventClient event(getType(), EVENT_ID_UPDATE_DATA);

    map<int, CArray<int,1> >::iterator it;
    list<CMessage> list_msg;
    list<CArray<double,1> > list_data;

    if (!grid->doGridHaveDataDistributed())
    {
       if (0 == client->clientRank)
       {
          for (it = grid->storeIndex_toSrv.begin(); it != grid->storeIndex_toSrv.end(); it++)
          {
            int rank = it->first;
            CArray<int,1>& index = it->second;

            list_msg.push_back(CMessage());
            list_data.push_back(CArray<double,1>(index.numElements()));

            CArray<double,1>& data_tmp = list_data.back();
            for (int n = 0; n < data_tmp.numElements(); n++) data_tmp(n) = data(index(n));

            list_msg.back() << getId() << data_tmp;
            event.push(rank, 1, list_msg.back());
          }
          client->sendEvent(event);
       } else client->sendEvent(event);
    }
    else
    {
      for (it = grid->storeIndex_toSrv.begin(); it != grid->storeIndex_toSrv.end(); it++)
      {
        int rank = it->first;
        CArray<int,1>& index = it->second;

        list_msg.push_back(CMessage());
        list_data.push_back(CArray<double,1>(index.numElements()));

        CArray<double,1>& data_tmp = list_data.back();
        for (int n = 0; n < data_tmp.numElements(); n++) data_tmp(n) = data(index(n));

        list_msg.back() << getId() << data_tmp;
        event.push(rank, grid->nbSenders[rank], list_msg.back());
      }
      client->sendEvent(event);
    }

    CTimer::get("XIOS Send Data").suspend();
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
      for (map<int, CArray<size_t, 1> >::iterator it = grid->outIndexFromClient.begin(); it != grid->outIndexFromClient.end(); ++it)
      {
        int rank = it->first;
        data_srv.insert(std::make_pair(rank, CArray<double,1>(it->second.numElements())));
        foperation_srv.insert(pair<int,boost::shared_ptr<func::CFunctor> >(rank,boost::shared_ptr<func::CFunctor>(new func::CInstant(data_srv[rank]))));
      }
    }

    CContext* context = CContext::getCurrent();
    const CDate& currDate = context->getCalendar()->getCurrentDate();
    const CDate opeDate      = last_operation_srv + freq_operation_srv;
    const CDate writeDate    = last_Write_srv     + freq_write_srv;

    if (opeDate <= currDate)
    {
      for (int n = 0; n < ranks.size(); n++)
      {
        CArray<double,1> data_tmp;
        *buffers[n] >> data_tmp;
        (*foperation_srv[ranks[n]])(data_tmp);
      }
      last_operation_srv = currDate;
    }

    if (writeDate < (currDate + freq_operation_srv))
    {
      for (int n = 0; n < ranks.size(); n++)
      {
        this->foperation_srv[ranks[n]]->final();
      }

      last_Write_srv = writeDate;
      writeField();
      lastlast_Write_srv = last_Write_srv;
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

    lastDataRequestedFromServer = context->getCalendar()->getCurrentDate();
    isReadDataRequestPending = true;

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

    map<int, CArray<double,1> >::iterator it;
    for (it = data_srv.begin(); it != data_srv.end(); it++)
    {
      msgs.push_back(CMessage());
      CMessage& msg = msgs.back();
      msg << getId();
      if (hasData)
        msg << getNStep() - 1 << it->second;
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
          for (map<int, CArray<size_t, 1> >::iterator it = grid->outIndexFromClient.begin(); it != grid->outIndexFromClient.end(); ++it)
            data_srv.insert(std::make_pair(it->first, CArray<double,1>(it->second.numElements())));
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
    std::map<int, CArray<double,1> > data;

    bool isEOF = false;

    for (int i = 0; i < ranks.size(); i++)
    {
      int rank = ranks[i];
      *buffers[i] >> record;
      isEOF = (record == size_t(-1));

      if (!isEOF)
        *buffers[i] >> data[rank];
      else
        break;
    }

    if (isEOF)
      serverSourceFilter->signalEndOfStream(lastDataRequestedFromServer);
    else
      serverSourceFilter->streamDataFromServer(lastDataRequestedFromServer, data);

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

   func::CFunctor::ETimeType CField::getOperationTimeType() const
   {
     return operationTimeType;
   }

   //----------------------------------------------------------------

   void CField::incrementNStep(void)
   {
      this->nstep++;
   }

   void CField::resetNStep(StdSize nstep /*= 0*/)
   {
      this->nstep = nstep;
   }

   void CField::resetNStepMax(void)
   {
      this->nstepMax = 0;
   }

   //----------------------------------------------------------------

   bool CField::isActive(void) const
   {
      return !this->refObject.empty();
   }

   //----------------------------------------------------------------

   bool CField::wasWritten() const
   {
     return written;
   }

   void CField::setWritten()
   {
     written = true;
   }

   //----------------------------------------------------------------

   bool CField::getUseCompressedOutput() const
   {
     return useCompressedOutput;
   }

   void CField::setUseCompressedOutput()
   {
     useCompressedOutput = true;
   }

   //----------------------------------------------------------------

   boost::shared_ptr<COutputPin> CField::getInstantDataFilter()
   {
     return instantDataFilter;
   }

   //----------------------------------------------------------------

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
          if (hasDirectFieldReference()) getDirectFieldReference()->solveAllReferenceEnabledField(false);
        }
        else if (context->hasServer)
          solveServerOperation();

        solveGridReference();
     }
     if (context->hasClient)
     {
       solveGenerateGrid();
     }

     solveGridDomainAxisRef(doSending2Sever);

     if (context->hasClient)
     {
       solveTransformedGrid();
     }

     solveCheckMaskIndex(doSending2Sever);
   }

   std::map<int, StdSize> CField::getGridAttributesBufferSize()
   {
     return grid->getAttributesBufferSize();
   }

   std::map<int, StdSize> CField::getGridDataBufferSize()
   {
     return grid->getDataBufferSize(getId());
   }

   //----------------------------------------------------------------

   void CField::solveServerOperation(void)
   {
      CContext* context = CContext::getCurrent();

      if (!context->hasServer || !hasOutputFile) return;

      if (freq_op.isEmpty())
        freq_op.setValue(TimeStep);

      if (freq_offset.isEmpty())
        freq_offset.setValue(NoneDu);

      freq_operation_srv = file->output_freq.getValue();
      freq_write_srv     = file->output_freq.getValue();

      lastlast_Write_srv = context->getCalendar()->getInitDate();
      last_Write_srv     = context->getCalendar()->getInitDate();
      last_operation_srv = context->getCalendar()->getInitDate();

      const CDuration toffset = freq_operation_srv - freq_offset.getValue() - context->getCalendar()->getTimeStep();
      last_operation_srv     = last_operation_srv - toffset;

      if (operation.isEmpty())
        ERROR("void CField::solveServerOperation(void)",
              << "An operation must be defined for field \"" << getId() << "\".");

      boost::shared_ptr<func::CFunctor> functor;
      CArray<double, 1> dummyData;

#define DECLARE_FUNCTOR(MType, mtype) \
      if (operation.getValue().compare(#mtype) == 0) \
      { \
        functor.reset(new func::C##MType(dummyData)); \
      }

#include "functor_type.conf"

      if (!functor)
        ERROR("void CField::solveServerOperation(void)",
              << "\"" << operation << "\" is not a valid operation.");

      operationTimeType = functor->timeType();
   }

   //----------------------------------------------------------------

   /*!
    * Constructs the graph filter for the field, enabling or not the data output.
    * This method should not be called more than once with enableOutput equal to true.
    *
    * \param gc the garbage collector to use when building the filter graph
    * \param enableOutput must be true when the field data is to be
    *                     read by the client or/and written to a file
    */
   void CField::buildFilterGraph(CGarbageCollector& gc, bool enableOutput)
   {
     if (!areAllReferenceSolved) solveAllReferenceEnabledField(false);

     // Start by building a filter which can provide the field's instant data
     if (!instantDataFilter)
     {
       // Check if we have an expression to parse
       if (!content.empty())
       {
         boost::scoped_ptr<IFilterExprNode> expr(parseExpr(content + '\0'));
         instantDataFilter = expr->reduce(gc, *this);
       }
       // Check if we have a reference on another field
       else if (!field_ref.isEmpty())
         instantDataFilter = getFieldReference(gc);
       // Check if the data is to be read from a file
       else if (file && !file->mode.isEmpty() && file->mode == CFile::mode_attr::read)
         instantDataFilter = serverSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(grid));
       else // The data might be passed from the model
         instantDataFilter = clientSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(grid));
     }

     // If the field data is to be read by the client or/and written to a file
     if (enableOutput && !storeFilter && !fileWriterFilter)
     {
       if (!read_access.isEmpty() && read_access.getValue())
       {
         storeFilter = boost::shared_ptr<CStoreFilter>(new CStoreFilter(gc, CContext::getCurrent(), grid));
         instantDataFilter->connectOutput(storeFilter, 0);
       }

       if (file && (file->mode.isEmpty() || file->mode == CFile::mode_attr::write))
       {
         fileWriterFilter = boost::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this));
         getTemporalDataFilter(gc, file->output_freq)->connectOutput(fileWriterFilter, 0);
       }
     }
   }

   /*!
    * Returns the filter needed to handle the field reference.
    * This method should only be called when building the filter graph corresponding to the field.
    *
    * \param gc the garbage collector to use
    * \return the output pin corresponding to the field reference
    */
   boost::shared_ptr<COutputPin> CField::getFieldReference(CGarbageCollector& gc)
   {
     if (instantDataFilter || field_ref.isEmpty())
       ERROR("COutputPin* CField::getFieldReference(CGarbageCollector& gc)",
             "Impossible to get the field reference for a field which has already been parsed or which does not have a field_ref.");

     CField* fieldRef = CField::get(field_ref);
     fieldRef->buildFilterGraph(gc, false);

     std::pair<boost::shared_ptr<CFilter>, boost::shared_ptr<CFilter> > filters;
     // Check if a spatial transformation is needed
     if (!grid_ref.isEmpty() && !fieldRef->grid_ref.isEmpty() && grid_ref.getValue() != fieldRef->grid_ref.getValue())
       filters = CSpatialTransformFilter::buildFilterGraph(gc, fieldRef->grid, grid);
     else
       filters.first = filters.second = boost::shared_ptr<CFilter>(new CPassThroughFilter(gc));

     fieldRef->getInstantDataFilter()->connectOutput(filters.first, 0);

     return filters.second;
   }

   /*!
    * Returns the filter needed to handle a self reference in the field's expression.
    * If the needed filter does not exist, it is created, otherwise it is reused.
    * This method should only be called when building the filter graph corresponding
    * to the field's expression.
    *
    * \param gc the garbage collector to use
    * \return the output pin corresponding to a self reference
    */
   boost::shared_ptr<COutputPin> CField::getSelfReference(CGarbageCollector& gc)
   {
     if (instantDataFilter || content.empty())
       ERROR("COutputPin* CField::getSelfReference(CGarbageCollector& gc)",
             "Impossible to add a self reference to a field which has already been parsed or which does not have an expression.");

     if (!selfReferenceFilter)
     {
       if (!field_ref.isEmpty())
         selfReferenceFilter = getFieldReference(gc);
       else
       {
         if (!clientSourceFilter)
           clientSourceFilter = boost::shared_ptr<CSourceFilter>(new CSourceFilter(grid));

         selfReferenceFilter = clientSourceFilter;
       }
     }

     return selfReferenceFilter;
   }

   /*!
    * Returns the temporal filter corresponding to the field's temporal operation
    * for the specified operation frequency. The filter is created if it does not
    * exist, otherwise it is reused.
    *
    * \param gc the garbage collector to use
    * \param outFreq the operation frequency, i.e. the frequency at which the output data will be computed
    * \return the output pin corresponding to the requested temporal filter
    */
   boost::shared_ptr<COutputPin> CField::getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)
   {
     std::map<CDuration, boost::shared_ptr<COutputPin> >::iterator it = temporalDataFilters.find(outFreq);

     if (it == temporalDataFilters.end())
     {
       if (operation.isEmpty())
         ERROR("void CField::getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)",
               << "An operation must be defined for field \"" << getId() << "\".");

       if (freq_op.isEmpty())
         freq_op.setValue(TimeStep);
       if (freq_offset.isEmpty())
         freq_offset.setValue(NoneDu);

       const bool ignoreMissingValue = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);

       boost::shared_ptr<CTemporalFilter> temporalFilter(new CTemporalFilter(gc, operation,
                                                                             CContext::getCurrent()->getCalendar()->getInitDate(),
                                                                             freq_op, freq_offset, outFreq,
                                                                             ignoreMissingValue, ignoreMissingValue ? default_value : 0.0));
       instantDataFilter->connectOutput(temporalFilter, 0);

       it = temporalDataFilters.insert(std::make_pair(outFreq, temporalFilter)).first;
     }

     return it->second;
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

   void CField::solveTransformedGrid()
   {
     if (!grid_ref.isEmpty() && hasDirectFieldReference() && !getDirectFieldReference()->grid_ref.isEmpty()
         && grid_ref.getValue() != getDirectFieldReference()->grid_ref.getValue() && !grid->isTransformed())
       grid->transformGrid(getDirectFieldReference()->grid);
   }

   void CField::solveGenerateGrid()
   {
     if (!grid_ref.isEmpty() && hasDirectFieldReference() && !getDirectFieldReference()->grid_ref.isEmpty()
         && grid_ref.getValue() != getDirectFieldReference()->grid_ref.getValue() && !grid->isTransformed())
       grid->completeGrid(getDirectFieldReference()->grid);
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
     map<int, CArray<double,1> >::iterator it;
     for (it = data_srv.begin(); it != data_srv.end(); it++) it->second = (it->second - addOffset) / scaleFactor;
   }

   void CField::invertScaleFactorAddOffset(double scaleFactor, double addOffset)
   {
     map<int, CArray<double,1> >::iterator it;
     for (it = data_srv.begin(); it != data_srv.end(); it++) it->second = it->second * scaleFactor + addOffset;
   }

   void CField::outputField(CArray<double,3>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;
      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
        grid->outputField(it->first, it->second, fieldOut.dataFirst());
      }
   }

   void CField::outputField(CArray<double,2>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;
      for(it=data_srv.begin();it!=data_srv.end();it++)
      {
         grid->outputField(it->first, it->second, fieldOut.dataFirst());
      }
   }

   void CField::outputField(CArray<double,1>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;

      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->outputField(it->first, it->second, fieldOut.dataFirst());
      }
   }

   void CField::inputField(CArray<double,3>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;
      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
        grid->inputField(it->first, fieldOut.dataFirst(), it->second);
      }
   }

   void CField::inputField(CArray<double,2>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;
      for(it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->inputField(it->first, fieldOut.dataFirst(), it->second);
      }
   }

   void CField::inputField(CArray<double,1>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;
      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->inputField(it->first, fieldOut.dataFirst(), it->second);
      }
   }

   void CField::outputCompressedField(CArray<double,1>& fieldOut)
   {
      map<int, CArray<double,1> >::iterator it;

      for (it = data_srv.begin(); it != data_srv.end(); it++)
      {
         grid->outputCompressedField(it->first, it->second, fieldOut.dataFirst());
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
} // namespace xios
