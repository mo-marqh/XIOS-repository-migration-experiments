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
#include "file_server_writer_filter.hpp"
#include "tracer.hpp"

namespace xios
{

  /// ////////////////////// Définitions ////////////////////// ///

  CField::CField(void)
    : CObjectTemplate<CField>(), CFieldAttributes()
    , written(false)
    , nstep(0), nstepMax(0)
    , hasOutputFile(false)
    , domAxisScalarIds_(vector<StdString>(3,""))
    , areAllReferenceSolved(false), isReferenceSolved(false), isReferenceSolvedAndTransformed(false)
    , isGridChecked(false)
    , useCompressedOutput(false)
    , hasTimeInstant(false)
    , hasTimeCentered(false)
    , wasDataRequestedFromServer(false)
    , wasDataAlreadyReceivedFromServer(false)
    , mustAutoTrigger(false)
    , isEOF(false), nstepMaxRead(false)
  { setVirtualVariableGroup(CVariableGroup::create(getId() + "_virtual_variable_group")); }

  CField::CField(const StdString& id)
    : CObjectTemplate<CField>(id), CFieldAttributes()
    , written(false)
    , nstep(0), nstepMax(0)
    , hasOutputFile(false)
    , domAxisScalarIds_(vector<StdString>(3,""))
    , areAllReferenceSolved(false), isReferenceSolved(false), isReferenceSolvedAndTransformed(false)
    , isGridChecked(false)
    , useCompressedOutput(false)
    , hasTimeInstant(false)
    , hasTimeCentered(false)
    , wasDataRequestedFromServer(false)
    , wasDataAlreadyReceivedFromServer(false)
    , mustAutoTrigger(false)
    , isEOF(false), nstepMaxRead(false)
  { setVirtualVariableGroup(CVariableGroup::create(getId() + "_virtual_variable_group")); }

  CField::~CField(void)
  {}

  //----------------------------------------------------------------

  void CField::setVirtualVariableGroup(CVariableGroup* newVVariableGroup)
  TRY
  {
    this->vVariableGroup = newVVariableGroup;
  }
  CATCH

  CVariableGroup* CField::getVirtualVariableGroup(void) const
  TRY
  {
     return this->vVariableGroup;
  }
  CATCH

  std::vector<CVariable*> CField::getAllVariables(void) const
  TRY
  {
    return this->vVariableGroup->getAllChildren();
  }
  CATCH

  void CField::solveDescInheritance(bool apply, const CAttributeMap* const parent)
  TRY
  {
    SuperClassAttribute::setAttributes(parent, apply);
    this->getVirtualVariableGroup()->solveDescInheritance(apply, NULL);
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------

  bool CField::dispatchEvent(CEventServer& event)
  TRY
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
     
        case EVENT_ID_GRID_COMPLETED :
          recvGridCompleted(event);
          return true;
          break;
        default :
          ERROR("bool CField::dispatchEvent(CEventServer& event)", << "Unknown Event");
          return false;
      }
    }
  }
  CATCH

  void CField::sendUpdateData(Time timeStamp, const CArray<double,1>& data, CContextClient* client)
  TRY
  {
    CTimer::get("Field : send data").resume();
    int receiverSize = client->serverSize;

    CEventClient event(getType(), EVENT_ID_UPDATE_DATA);

    map<int, CArray<int,1> >::iterator it;
    list<CMessage> list_msg;
    list<CArray<double,1> > list_data;

    if (!grid_->doGridHaveDataDistributed(client))
    {
       if (client->isServerLeader())
       {
          for (it = grid_->storeIndex_toSrv_[client].begin(); it != grid_->storeIndex_toSrv_[client].end(); it++)
          {
            int rank = it->first;
            CArray<int,1>& index = it->second;

            list_msg.push_back(CMessage());
            list_data.push_back(CArray<double,1>(index.numElements()));

            CArray<double,1>& data_tmp = list_data.back();
            for (int n = 0; n < data_tmp.numElements(); n++) data_tmp(n) = data(index(n));

            list_msg.back() << getId() << timeStamp << data_tmp;
            event.push(rank, 1, list_msg.back());
          }
          client->sendEvent(event);
        }
      else client->sendEvent(event);
    }
    else
    {
      for (it = grid_->storeIndex_toSrv_[client].begin(); it != grid_->storeIndex_toSrv_[client].end(); it++)
      {
        int rank = it->first;
        CArray<int,1>& index = it->second;

        list_msg.push_back(CMessage());
        list_data.push_back(CArray<double,1>(index.numElements()));

        CArray<double,1>& data_tmp = list_data.back();
        for (int n = 0; n < data_tmp.numElements(); n++) data_tmp(n) = data(index(n));

        list_msg.back() << getId() << timeStamp << data_tmp;
        event.push(rank, grid_->nbSenders_[receiverSize][rank], list_msg.back());
      }
      client->sendEvent(event);
    }

    CTimer::get("Field : send data").suspend();
  }
  CATCH_DUMP_ATTR

  void CField::recvUpdateData(CEventServer& event)
  TRY
  {
    std::map<int,CBufferIn*> rankBuffers;

    list<CEventServer::SSubEvent>::iterator it;
    string fieldId;
    CTimer::get("Field : recv data").resume();
    for (it = event.subEvents.begin(); it != event.subEvents.end(); ++it)
    {
      int rank = it->rank;
      CBufferIn* buffer = it->buffer;
      *buffer >> fieldId;
      rankBuffers[rank] = buffer;
    }
    get(fieldId)->recvUpdateData(rankBuffers);
    CTimer::get("Field : recv data").suspend();
  }
  CATCH


  void  CField::recvUpdateData(std::map<int,CBufferIn*>& rankBuffers)
  TRY
  {
    if (hasCouplerIn()) recvUpdateDataFromCoupler(rankBuffers) ;
    else recvUpdateDataFromClient(rankBuffers) ;
  }
  CATCH

  void  CField::recvUpdateDataFromClient(std::map<int,CBufferIn*>& rankBuffers)
  TRY
  {
    CContext* context = CContext::getCurrent();
    Time timeStamp ;
    size_t sizeData = 0;
    if (0 == recvDataSrv.numElements())
    {            
      CArray<int,1>& storeClient = grid_->getStoreIndex_client();

      // Gather all data from different clients      
      recvDataSrv.resize(storeClient.numElements());
      recvFoperationSrv = std::shared_ptr<func::CFunctor>(new func::CInstant(recvDataSrv));
    }

    CArray<double,1> recv_data_tmp(recvDataSrv.numElements());    
    const CDate& currDate = context->getCalendar()->getCurrentDate();
    CDuration offsetAllButMonth (freq_offset.getValue().year, 0 , freq_offset.getValue().day,
                                 freq_offset.getValue().hour, freq_offset.getValue().minute,
                                 freq_offset.getValue().second, freq_offset.getValue().timestep);
    const CDate opeDate   = (last_operation_srv - offsetAllButMonth + context->getCalendar()->getTimeStep())
                             + freq_op + freq_operation_srv - freq_op - context->getCalendar()->getTimeStep() + offsetAllButMonth;

    if (opeDate <= currDate)
    {
      auto& outLocalIndexStoreOnClient = grid_-> getOutLocalIndexStoreOnClient() ;
      for (auto it = outLocalIndexStoreOnClient.begin(); it != outLocalIndexStoreOnClient.end(); ++it)
      {
        CArray<double,1> tmp;
        CArray<size_t,1>& indexTmp = it->second;
        *(rankBuffers[it->first]) >> timeStamp >> tmp;
        for (int idx = 0; idx < indexTmp.numElements(); ++idx) recv_data_tmp(indexTmp(idx)) = tmp(idx);
      }
    }

    this->setData(recv_data_tmp);
    // delete incomming flux for server only
    recvFoperationSrv.reset() ;
    recvDataSrv.reset() ;
  }
  CATCH_DUMP_ATTR

  void CField::writeUpdateData(const CArray<double,1>& data)
  TRY
  {
    CContext* context = CContext::getCurrent();

    const CDate& currDate = context->getCalendar()->getCurrentDate();
    CDuration offsetAllButMonth (freq_offset.getValue().year, 0 , freq_offset.getValue().day,
                                   freq_offset.getValue().hour, freq_offset.getValue().minute,
                                   freq_offset.getValue().second, freq_offset.getValue().timestep);
    const CDate opeDate   = (last_operation_srv - offsetAllButMonth + context->getCalendar()->getTimeStep())
                              + freq_op + freq_operation_srv - freq_op - context->getCalendar()->getTimeStep() + offsetAllButMonth;
    const CDate writeDate = last_Write_srv + freq_write_srv;

    if (opeDate <= currDate)
    {
      (*recvFoperationSrv)(data);
      last_operation_srv = currDate;
    }

    if (writeDate < (currDate + freq_operation_srv))
    {
      recvFoperationSrv->final();
      last_Write_srv = writeDate;
      grid_->computeWrittenIndex();
      writeField();
      lastlast_Write_srv = last_Write_srv;
    }
  }
  CATCH_DUMP_ATTR

  void CField::writeField(void)
  TRY
  {
    if (!getRelFile()->isEmptyZone())
    {
      if (grid_->doGridHaveDataToWrite() || getRelFile()->type == CFile::type_attr::one_file)
      {
        getRelFile()->checkWriteFile();
        this->incrementNStep();
        getRelFile()->getDataOutput()->writeFieldData(CField::get(this));
      }
    }
  }
  CATCH_DUMP_ATTR

  /*
    Send a request for reading data.
    Client sends a request to server for demanding server to read data and send back to it.
    For now, this function is called only by client
    In the future, it can be called by level-1 servers
    \param [in] tsDataRequested timestamp when the call is made
  */
  bool CField::sendReadDataRequest(const CDate& tsDataRequested, CContextClient* client)
  TRY
  {
    CContext* context = CContext::getCurrent();

    lastDataRequestedFromServer = tsDataRequested;

    // No need to send the request if we are sure that we are already at EOF
    if (!isEOF || context->getCalendar()->getCurrentDate() <= dateEOF)
    {
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
    else serverSourceFilter->signalEndOfStream(tsDataRequested);

    wasDataRequestedFromServer = true;

    return !isEOF;
  }
  CATCH_DUMP_ATTR

  /*!
  Send request new data read from file if need be, that is the current data is out-of-date.
  \return true if and only if some data was requested
  */
  bool CField::sendReadDataRequestIfNeeded(void)
  TRY
  {
    const CDate& currentDate = CContext::getCurrent()->getCalendar()->getCurrentDate();

    bool dataRequested = false;

    while (currentDate >= lastDataRequestedFromServer)
    {
      info(20) << "currentDate : " << currentDate << endl ;
      info(20) << "lastDataRequestedFromServer : " << lastDataRequestedFromServer << endl ;
      info(20) << "fileIn_->output_freq.getValue() : " << fileIn_->output_freq.getValue() << endl ;
      info(20) << "lastDataRequestedFromServer + fileIn_->output_freq.getValue() : " << lastDataRequestedFromServer + fileIn_->output_freq << endl ;

      dataRequested |= sendReadDataRequest(lastDataRequestedFromServer + fileIn_->output_freq, fileIn_->getContextClient());
    }

    return dataRequested;
  }
  CATCH_DUMP_ATTR

  void CField::recvReadDataRequest(CEventServer& event)
  TRY
  {
    CBufferIn* buffer = event.subEvents.begin()->buffer;
    StdString fieldId;
    *buffer >> fieldId;
    get(fieldId)->recvReadDataRequest(event.getContextServer());
  }
  CATCH

  /*!
    Receive data request sent from client and process it
    Every time server receives this request, it will try to read data and sent read data back to client
    At the moment, this function is called by server level 1
    In the future, this should (only) be done by the last level servers.
  */
  void CField::recvReadDataRequest(CContextServer* server)
  TRY
  {
    CContextClient* client = server->getAssociatedClient() ;
    CEventClient event(getType(), EVENT_ID_READ_DATA_READY);
    std::list<CMessage> msgs;

    EReadField hasData = readField();

    map<int, CArray<double,1> >::iterator it;
    if (!grid_->doGridHaveDataDistributed(client))
    {
       if (client->isServerLeader())
       {
          if (0 != recvDataSrv.numElements())
          {            
            const std::list<int>& ranks = client->getRanksServerLeader();
            for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
            {
              msgs.push_back(CMessage());
              CMessage& msg = msgs.back();
              msg << getId();
              switch (hasData)
              {
                case RF_DATA:
                  msg << getNStep() - 1 << recvDataSrv;
                  break;
                case RF_NODATA:
                  msg << int(-2) << recvDataSrv;
                  break;
                case RF_EOF:                  
                default:
                  msg << int(-1);
                  break;
              }

              event.push(*itRank, 1, msg);
            }
          }
          client->sendEvent(event);
       }
       else
       {
          client->sendEvent(event);
       }
    }
    else
    {
      auto& outLocalIndexStoreOnClient = grid_-> getOutLocalIndexStoreOnClient() ;
      for (auto it = outLocalIndexStoreOnClient.begin(); it != outLocalIndexStoreOnClient.end(); ++it)
      {
        CArray<size_t,1>& indexTmp = it->second;
        CArray<double,1> tmp(indexTmp.numElements());
        for (int idx = 0; idx < indexTmp.numElements(); ++idx)
        {
          tmp(idx) = recvDataSrv(indexTmp(idx));
        } 

        msgs.push_back(CMessage());
        CMessage& msg = msgs.back();
        msg << getId();
        switch (hasData)
        {
          case RF_DATA:
            msg << getNStep() - 1 << tmp;
            break;
          case RF_NODATA:
            msg << int(-2) << tmp;
            break;
          case RF_EOF:                  
          default:
            msg << int(-1);
            break;
        }

        event.push(it->first, grid_->getNbReadSenders(client)[it->first], msg);
      }
      client->sendEvent(event);
    }
  }
  CATCH_DUMP_ATTR

  /*!
    Read field from a file.
    A field is read with the distribution of data on the server side
    \return State of field can be read from a file
  */
  CField::EReadField CField::readField(void)
  TRY
  {
    CContext* context = CContext::getCurrent();
    grid_->computeWrittenIndex();
    getRelFile()->initRead();
    EReadField readState = RF_DATA;

    if (!getRelFile()->isEmptyZone())
    {      
      if (grid_->doGridHaveDataToWrite() || getRelFile()->type == CFile::type_attr::one_file)      
      {
        if (0 == recvDataSrv.numElements())
        {            
          CArray<int,1>& storeClient = grid_->getStoreIndex_client();          
          recvDataSrv.resize(storeClient.numElements());          
        }
        
        getRelFile()->checkReadFile();

        if (!nstepMax)
        {
          nstepMax = getRelFile()->getDataInput()->getFieldNbRecords(CField::get(this));
        }

        this->incrementNStep();

        if (getNStep() > nstepMax && (getRelFile()->cyclic.isEmpty() || !getRelFile()->cyclic) )
          readState = RF_EOF;

        if (RF_EOF != readState)
          getRelFile()->getDataInput()->readFieldData(CField::get(this));
      }
    }
    else
    {
      this->incrementNStep();
      if (getNStep() > nstepMax && (getRelFile()->cyclic.isEmpty() || !getRelFile()->cyclic) )
        readState = RF_EOF;
      else
        readState = RF_NODATA;

      if (!nstepMaxRead) // This can be a bug if we try to read field from zero time record
        readState = RF_NODATA;
    }

    if (!nstepMaxRead)
    {
       MPI_Allreduce(MPI_IN_PLACE, &nstepMax, 1, MPI_INT, MPI_MAX, context->intraComm_);
       nstepMaxRead = true;
    }

    return readState;
  }
  CATCH_DUMP_ATTR

  /*
    Receive read data from server.
    At the moment, this function is called in the client side.
    In the future, this function can be called hiearachically (server n-1, server n -2, ..., client)
    \param event event containing read data
  */
  void CField::recvReadDataReady(CEventServer& event)
  TRY
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
  CATCH

  
  void CField::recvUpdateDataFromCoupler(std::map<int,CBufferIn*>& rankBuffers)
  TRY
  {
    CContext* context = CContext::getCurrent();
    Time timeStamp ;
    if (wasDataAlreadyReceivedFromServer)
    {  
      lastDataReceivedFromServer = lastDataReceivedFromServer + freq_op;
    }
    else
    {
      // unlikely to input from file server where data are received at ts=0
      // for coupling, it would be after the first freq_op, because for now we don't have
      // restart mecanism to send the value at ts=0. It mus be changed in future
      lastDataReceivedFromServer = context->getCalendar()->getInitDate();
      wasDataAlreadyReceivedFromServer = true;
    }

    CArray<int,1>& storeClient = grid_->getStoreIndex_client();
    CArray<double,1> recv_data_tmp(storeClient.numElements());  

    auto& outLocalIndexStoreOnClient = grid_-> getOutLocalIndexStoreOnClient() ;
    for (auto it = outLocalIndexStoreOnClient.begin(); it != outLocalIndexStoreOnClient.end(); ++it)
    {
      CArray<double,1> tmp;
      CArray<size_t,1>& indexTmp = it->second;
      *(rankBuffers[it->first]) >> timeStamp >> tmp;
      for (int idx = 0; idx < indexTmp.numElements(); ++idx) recv_data_tmp(indexTmp(idx)) = tmp(idx);
    }
    
    clientSourceFilter->streamData(lastDataReceivedFromServer, recv_data_tmp);
    
  }
  CATCH_DUMP_ATTR
  /*!
    Receive read data from server
    \param [in] ranks Ranks of sending processes
    \param [in] buffers buffers containing read data
  */
  void CField::recvReadDataReady(vector<int> ranks, vector<CBufferIn*> buffers)
  TRY
  {
    CContext* context = CContext::getCurrent();
    std::map<int, CArray<double,1> > data;
    const bool wasEOF = isEOF;

    for (int i = 0; i < ranks.size(); i++)
    {
      int rank = ranks[i];
      int record;
      *buffers[i] >> record;
      isEOF = (record == int(-1));

      if (!isEOF)
        *buffers[i] >> data[rank];
      else
        break;
    }

    if (wasDataAlreadyReceivedFromServer)
      lastDataReceivedFromServer = lastDataReceivedFromServer + fileIn_->output_freq;
    else
    {
      lastDataReceivedFromServer = context->getCalendar()->getInitDate();
      wasDataAlreadyReceivedFromServer = true;
    }

    if (isEOF)
    {
      if (!wasEOF)
        dateEOF = lastDataReceivedFromServer;

      serverSourceFilter->signalEndOfStream(lastDataReceivedFromServer);
    }
    else
      serverSourceFilter->streamDataFromServer(lastDataReceivedFromServer, data);
  }
  CATCH_DUMP_ATTR

  void CField::checkForLateDataFromCoupler(void)
  TRY
  {
    CContext* context = CContext::getCurrent();
    const CDate& currentDate = context->getCalendar()->getCurrentDate();

    CTimer timer("CField::checkForLateDataFromCoupler");
    timer.resume();
    traceOff() ;
    timer.suspend();
      
    bool isDataLate;
    do
    {
      if (wasDataAlreadyReceivedFromServer) isDataLate = lastDataReceivedFromServer + freq_offset + freq_op <= currentDate ;
      else isDataLate = context->getCalendar()->getInitDate()+freq_offset <= currentDate ;

      if (isDataLate)
      {
        timer.resume();
//ym          context->checkBuffersAndListen();
//ym            context->eventLoop();
        context->globalEventLoop();

        timer.suspend();
      }
    } while (isDataLate && timer.getCumulatedTime() < CXios::recvFieldTimeout);
    
    timer.resume();
    traceOn() ;
    timer.suspend() ;

    if (isDataLate) ERROR("void CField::checkForLateDataFromCoupler(void)",
                            << "Late data at timestep = " << currentDate);
  }
  CATCH_DUMP_ATTR


  void CField::checkForLateDataFromServer(void)
  TRY
  {
    CContext* context = CContext::getCurrent();
    const CDate& currentDate = context->getCalendar()->getCurrentDate();

    // Check if data previously requested has been received as expected
    if (wasDataRequestedFromServer && !isEOF)
    {
      CTimer timer("CField::checkForLateDataFromServer");
      timer.resume();
      traceOff() ;
      timer.suspend();
      
      bool isDataLate;
      do
      {
        const CDate nextDataDue = wasDataAlreadyReceivedFromServer ? (lastDataReceivedFromServer + fileIn_->output_freq) : context->getCalendar()->getInitDate();
        isDataLate = (nextDataDue <= currentDate);

        if (isDataLate)
        {
          timer.resume();

//ym          context->checkBuffersAndListen();
//ym            context->eventLoop();
          context->globalEventLoop();

          timer.suspend();
        }
      }
      while (isDataLate && timer.getCumulatedTime() < CXios::recvFieldTimeout);
      timer.resume();
      traceOn() ;
      timer.suspend() ;


      if (isDataLate)
        ERROR("void CField::checkForLateDataFromServer(void)",
              << "Late data at timestep = " << currentDate);
    }
  }
  CATCH_DUMP_ATTR

  void CField::triggerLateField(void)
  TRY
  {
    if (hasFileIn()) 
    {
      checkForLateDataFromServer() ;
      serverSourceFilter->trigger(CContext::getCurrent()->getCalendar()->getCurrentDate()) ;
    } 
    else if (hasCouplerIn())
    {
      checkForLateDataFromCoupler() ;
      clientSourceFilter->trigger(CContext::getCurrent()->getCalendar()->getCurrentDate()) ;
    }
  }
  CATCH_DUMP_ATTR


  void CField::checkIfMustAutoTrigger(void)
  TRY
  {
    mustAutoTrigger = serverSourceFilter ? serverSourceFilter->mustAutoTrigger() : false;
  }
  CATCH_DUMP_ATTR

  void CField::autoTriggerIfNeeded(void)
  TRY
  {
    if (mustAutoTrigger)
      serverSourceFilter->trigger(CContext::getCurrent()->getCalendar()->getCurrentDate());
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------
/*
  void CField::setRelFile(CFile* _file)
  TRY
  {
    this->file = _file;
    hasOutputFile = true;
  }
  CATCH_DUMP_ATTR
*/
  //----------------------------------------------------------------

  StdString CField::GetName(void)    { return StdString("field"); }
  StdString CField::GetDefName(void) { return CField::GetName(); }
  ENodeType CField::GetType(void)    { return eField; }

  //----------------------------------------------------------------

  CGrid* CField::getRelGrid(void) const
  TRY
  {
    return this->grid_;
  }
  CATCH

  //----------------------------------------------------------------

  CFile* CField::getRelFile(void) const
  TRY
  {
    if (hasFileIn()) return this->fileIn_;
    else if (hasFileOut()) return this->fileOut_ ;
    else return nullptr ;
  }
  CATCH

  int CField::getNStep(void) const
  TRY
  {
    return this->nstep;
  }
  CATCH

  func::CFunctor::ETimeType CField::getOperationTimeType() const
  TRY
  {
    return operationTimeType;
  }
  CATCH

   //----------------------------------------------------------------

  void CField::incrementNStep(void)
  TRY
  {
    this->nstep++;
  }
  CATCH_DUMP_ATTR

  void CField::resetNStep(int nstep /*= 0*/)
  TRY
  {
    this->nstep = nstep;
  }
  CATCH_DUMP_ATTR

  void CField::resetNStepMax(void)
  TRY
  {
    this->nstepMax = 0;
    nstepMaxRead = false;
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------

  bool CField::isActive(bool atCurrentTimestep /*= false*/) const
  TRY
  {
    if (clientSourceFilter)
      return atCurrentTimestep ? clientSourceFilter->isDataExpected(CContext::getCurrent()->getCalendar()->getCurrentDate()) : true;
    else if (storeFilter)
      return true;
    else if (instantDataFilter)
      ERROR("bool CField::isActive(bool atCurrentTimestep)",
            << "Impossible to check if field [ id = " << getId() << " ] is active as it cannot be used to receive nor send data.");

    return false;
  }
  CATCH

  //----------------------------------------------------------------

  bool CField::wasWritten() const
  TRY
  {
    return written;
  }
  CATCH

  void CField::setWritten()
  TRY
  {
    written = true;
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------

  bool CField::getUseCompressedOutput() const
  TRY
  {
    return useCompressedOutput;
  }
  CATCH

  void CField::setUseCompressedOutput()
  TRY
  {
    useCompressedOutput = true;
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------

  std::shared_ptr<COutputPin> CField::getInstantDataFilter()
  TRY
  {
    return instantDataFilter;
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------

  /*!
    Build up graph of grids which plays role of destination and source in grid transformation
    This function should be called before \func solveGridReference()
  */
  void CField::buildGridTransformationGraph()
  TRY
  {
    CContext* context = CContext::getCurrent();
    if (context->getServiceType()==CServicesManager::CLIENT)
    {
      if (grid_ && !grid_->isTransformed() && hasDirectFieldReference() && grid_ != getDirectFieldReference()->grid_)
      {
        grid_->addTransGridSource(getDirectFieldReference()->grid_);
      }
    }
  }
  CATCH_DUMP_ATTR

  /*!
    Generate a new grid destination if there are more than one grid source pointing to a same grid destination
  */
  void CField::generateNewTransformationGridDest()
  TRY
  {
    CContext* context = CContext::getCurrent();
    if (context->getServiceType()==CServicesManager::CLIENT)
    {
      std::map<CGrid*,std::pair<bool,StdString> >& gridSrcMap = grid_->getTransGridSource();
      if (1 < gridSrcMap.size())
      {
        // Search for grid source
        CGrid* gridSrc = grid_;
        CField* currField = this;
        std::vector<CField*> hieraField;
        while (currField->hasDirectFieldReference() && (gridSrc == grid_))
        {
          hieraField.push_back(currField);
          CField* tmp = currField->getDirectFieldReference();
          currField = tmp;
          gridSrc = currField->grid_;
        }

        if (gridSrcMap.end() != gridSrcMap.find(gridSrc))
        {
          CGrid* gridTmp;
          std::pair<bool,StdString> newGridDest = gridSrcMap[gridSrc];
          if (newGridDest.first)
          {
            StdString newIdGridDest = newGridDest.second;
            if (!CGrid::has(newIdGridDest))
            {
              ERROR("CGrid* CGrid::generateNewTransformationGridDest()",
                << " Something wrong happened! Grid whose id " << newIdGridDest
                << "should exist ");
            }
            gridTmp = CGrid::get(newIdGridDest);
          }
          else
          {
            StdString newIdGridDest = CGrid::generateId(gridSrc, grid_);
            gridTmp = CGrid::cloneGrid(newIdGridDest, grid_);

            (gridSrcMap[gridSrc]).first = true;
            (gridSrcMap[gridSrc]).second = newIdGridDest;
          }

          // Update all descendants
          for (std::vector<CField*>::iterator it = hieraField.begin(); it != hieraField.end(); ++it)
          {
            (*it)->grid_ = gridTmp;
            (*it)->updateRef((*it)->grid_);
          }
        }
      }
    }
  }
  CATCH_DUMP_ATTR

  void CField::updateRef(CGrid* grid)
  TRY
  {
    if (!grid_ref.isEmpty()) grid_ref.setValue(grid->getId());
    else
    {
      std::vector<CAxis*> axisTmp = grid->getAxis();
      std::vector<CDomain*> domainTmp = grid->getDomains();
      if ((1<axisTmp.size()) || (1<domainTmp.size()))
        ERROR("void CField::updateRef(CGrid* grid)",
          << "More than one domain or axis is available for domain_ref/axis_ref of field " << this->getId());

      if ((!domain_ref.isEmpty()) && (domainTmp.empty()))
        ERROR("void CField::updateRef(CGrid* grid)",
          << "Incoherent between available domain and domain_ref of field " << this->getId());
      if ((!axis_ref.isEmpty()) && (axisTmp.empty()))
        ERROR("void CField::updateRef(CGrid* grid)",
          << "Incoherent between available axis and axis_ref of field " << this->getId());

      if (!domain_ref.isEmpty()) domain_ref.setValue(domainTmp[0]->getId());
      if (!axis_ref.isEmpty()) axis_ref.setValue(axisTmp[0]->getId());
    }
  }
  CATCH_DUMP_ATTR
   
  /*!
    Solve reference of all enabled fields even the source fields .
    In this step, we do transformations.
  */
  void CField::solveAllEnabledFieldsAndTransform()
  TRY
  {
    CContext* context = CContext::getCurrent();

    if (!isReferenceSolvedAndTransformed)
    {
      isReferenceSolvedAndTransformed = true;

      if (context->getServiceType()==CServicesManager::CLIENT)
      {
        solveRefInheritance(true);
        if (hasDirectFieldReference()) getDirectFieldReference()->solveAllEnabledFieldsAndTransform();
      }

      if (context->getServiceType()==CServicesManager::GATHERER || context->getServiceType()==CServicesManager::OUT_SERVER)
        solveServerOperation();

      solveGridReference();

      if (context->getServiceType()==CServicesManager::CLIENT)
      {
        solveGenerateGrid();
        buildGridTransformationGraph();
      }

      solveGridDomainAxisRef(false);

      if (context->getServiceType()==CServicesManager::CLIENT)
      {
        solveTransformedGrid();
      }

      solveGridDomainAxisRef(false);
    }
  }
  CATCH_DUMP_ATTR

  void CField::checkGridOfEnabledFields()
  TRY
  {
    if (!isGridChecked)
    {
      isGridChecked = true;
      solveCheckMaskIndex(false);
    }
  }
  CATCH_DUMP_ATTR

  void CField::sendGridComponentOfEnabledFields()
  TRY
  {
    solveGridDomainAxisRef(true);
    // solveCheckMaskIndex(true);
  }
  CATCH_DUMP_ATTR

  void CField::sendGridOfEnabledFields()
  TRY
  {
    // solveGridDomainAxisRef(true);
    solveCheckMaskIndex(true);
  }   
  CATCH_DUMP_ATTR

  void CField::solveOnlyReferenceEnabledField(void)
  TRY
  {
    CContext* context = CContext::getCurrent();
    if (!isReferenceSolved)
    {
      isReferenceSolved = true;

      if (context->getServiceType()==CServicesManager::CLIENT)
      {
        solveRefInheritance(true);
        if (hasDirectFieldReference()) getDirectFieldReference()->solveOnlyReferenceEnabledField();
      }

      if (context->getServiceType()==CServicesManager::GATHERER || context->getServiceType()==CServicesManager::OUT_SERVER)
        solveServerOperation();

      solveGridReference();
      grid_->solveElementsRefInheritance(true); // make it again to solve grid reading from file

      if (context->getServiceType()==CServicesManager::CLIENT)
      {
        solveGenerateGrid();
        buildGridTransformationGraph();
      }
    }
  }
  CATCH_DUMP_ATTR

  void CField::solveAllReferenceEnabledField(bool doSending2Server)
  TRY
  {
    CContext* context = CContext::getCurrent();
    solveOnlyReferenceEnabledField();

    if (!areAllReferenceSolved)
    {
      areAllReferenceSolved = true;
      
      if (context->getServiceType()==CServicesManager::CLIENT)
      {
        solveRefInheritance(true);
        if (hasDirectFieldReference()) getDirectFieldReference()->solveAllReferenceEnabledField(false);
      }
      else if (context->getServiceType()==CServicesManager::GATHERER || context->getServiceType()==CServicesManager::OUT_SERVER)
        solveServerOperation();

      solveGridReference();
    }

    solveGridDomainAxisRef(doSending2Server);

    if (context->getServiceType()==CServicesManager::CLIENT)
    {
      solveTransformedGrid();
    }

    solveCheckMaskIndex(doSending2Server);
  }
  CATCH_DUMP_ATTR

  /*!
   * Compute the required buffer size to send the fields data.
   * \param [in/out] bufferSize Modifying the bufferSize for the client context
   * \param [in/out] maxEventSize Modifying the maximum event size for the client context 
   * \param [in] bufferForWriting True if buffers are used for sending data for writing
   */  
  void CField::setContextClientDataBufferSize(map<CContextClient*,map<int,size_t>>& bufferSize, 
                                              map<CContextClient*,map<int,size_t>>& maxEventSize, 
                                              bool bufferForWriting)
  {
    auto& contextBufferSize = bufferSize[client] ;
    auto& contextMaxEventSize = maxEventSize[client] ;
    const std::map<int, size_t> mapSize = grid_->getDataBufferSize(client, getId(), bufferForWriting);
    for(auto& it : mapSize )
    {
      // If contextBufferSize[it.first] does not exist, it will be zero-initialized
      // so we can use it safely without checking for its existance
      if (CXios::isOptPerformance) contextBufferSize[it.first] += it.second;
      else if (contextBufferSize[it.first] < it.second) contextBufferSize[it.first] = it.second;

      if (contextMaxEventSize[it.first] < it.second) contextMaxEventSize[it.first] = it.second;
    }

  }

  void CField::setContextClientAttributesBufferSize(map<CContextClient*,map<int,size_t>>& bufferSize, 
                                                   map<CContextClient*,map<int,size_t>>& maxEventSize, 
                                                   bool bufferForWriting)
  {
    auto& contextBufferSize = bufferSize[client] ;
    auto& contextMaxEventSize = maxEventSize[client] ;
    const std::map<int, size_t> mapSize = grid_->getAttributesBufferSize(client, bufferForWriting);
    for(auto& it : mapSize )
    {
      // If contextBufferSize[it.first] does not exist, it will be zero-initialized
      // so we can use it safely without checking for its existance
      if (contextBufferSize[it.first] < it.second) contextBufferSize[it.first] = it.second;
      if (contextMaxEventSize[it.first] < it.second) contextMaxEventSize[it.first] = it.second;
    }

  }


// ym obsolete to be removed 
  std::map<int, StdSize> CField::getGridAttributesBufferSize(CContextClient* client, bool bufferForWriting /*= "false"*/)
  TRY
  {
    return grid_->getAttributesBufferSize(client, bufferForWriting);
  }
  CATCH_DUMP_ATTR

// ym obsolete to be removed 
  std::map<int, StdSize> CField::getGridDataBufferSize(CContextClient* client, bool bufferForWriting /*= "false"*/)
  TRY
  {
    return grid_->getDataBufferSize(client, getId(), bufferForWriting);
  }
  CATCH_DUMP_ATTR



  size_t CField::getGlobalWrittenSize()
  TRY
  {
    return grid_->getGlobalWrittenSize();
  }
  CATCH_DUMP_ATTR

  //----------------------------------------------------------------

  void CField::solveServerOperation(void)
  TRY
  {
    CContext* context = CContext::getCurrent();

    if (freq_op.isEmpty()) freq_op.setValue(TimeStep);

    if (freq_offset.isEmpty()) freq_offset.setValue(NoneDu);

    freq_operation_srv = fileOut_->output_freq.getValue();
    freq_write_srv     = fileOut_->output_freq.getValue();

    lastlast_Write_srv = context->getCalendar()->getInitDate();
    last_Write_srv     = context->getCalendar()->getInitDate();
    last_operation_srv = context->getCalendar()->getInitDate();

    const CDuration toffset = freq_operation_srv - freq_offset.getValue() - context->getCalendar()->getTimeStep();
    last_operation_srv     = last_operation_srv - toffset;

    if (operation.isEmpty())
      ERROR("void CField::solveServerOperation(void)",
            << "An operation must be defined for field \"" << getId() << "\".");

    std::shared_ptr<func::CFunctor> functor;
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
  CATCH_DUMP_ATTR

 //----------------------------------------------------------------

  bool CField::buildWorkflowGraph(CGarbageCollector& gc)
  {
    if (buildWorkflowGraphDone_) return true ;
    
    const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
    const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

    if (!inputFilter) inputFilter = std::shared_ptr<CPassThroughFilter>(new CPassThroughFilter(gc)); 
     
    if (hasDirectFieldReference())
    {
      CField* fieldRef = getDirectFieldReference();
      bool ret=fieldRef->buildWorkflowGraph(gc); 
      if (!ret) return false ; // workflow graph cannot be built at this stage
    }

    // now construct grid and check if element are enabled
    solveGridReference() ; // grid_ is now defined
    if (!isGridCompleted()) return false;

    // Check if we have an expression to parse
    std::shared_ptr<COutputPin> filterExpr ;
    if (hasExpression())
    {
      boost::scoped_ptr<IFilterExprNode> expr(parseExpr(getExpression() + '\0'));
      filterExpr = expr->reduce(gc, *this);
      if (!filterExpr) return false ; // workflow graph cannot be built at this stage
    }
    
    // prepare transformation. Need to know before if workflow of auxillary field can be built
    if (hasDirectFieldReference())
    {
      auto gridPath=getGridPath() ;
      gridPath.push_back(grid_) ;

      CGrid* gridSrc=getDirectFieldReference()->getGrid() ;
      for(auto grid : gridPath)
      {
        grid->solveElementsRefInheritance() ;
        grid->completeGrid(gridSrc); // grid generation, to be checked
        grid->checkElementsAttributes() ;
        grid->prepareTransformGrid(gridSrc) ; // prepare the grid tranformation
        for(auto fieldId : grid->getAuxInputTransformGrid()) // try to build workflow graph for auxillary field tranformation
          if (!CField::get(fieldId)->buildWorkflowGraph(gc)) return false ;
        gridSrc=grid ;
      }
      
      std::shared_ptr<COutputPin> lastFilter ;
      if (filterExpr) lastFilter=filterExpr ;
      else lastFilter = inputFilter ;
      
      gridSrc=getDirectFieldReference()->getGrid() ;
      for(auto grid : gridPath) 
      {
        grid->makeTransformGrid() ; // make the grid transformation
        if (grid->hasTransform()) 
        {
          std::pair<std::shared_ptr<CFilter>, std::shared_ptr<CFilter> > filters = CSpatialTransformFilter::buildFilterGraph(gc, gridSrc, grid, detectMissingValues, defaultValue); 
          lastFilter->connectOutput(filters.first, 0);
          lastFilter = filters.second;
          gridSrc=grid ;
        }
      }
      instantDataFilter = lastFilter ;
      
      // connect the input Filter to the reference
      getDirectFieldReference()->getInstantDataFilter()->connectOutput(inputFilter,0);
    }
    else 
    {
      if (hasFileIn()) // input file, attemp to read the grid from file
      {
         // must be checked
         fileIn_->initRead() ;
         fileIn_->checkReadFile();
         grid_->solveElementsRefInheritance() ;
         if (fileIn_->isClientSide()) fileIn_->readFieldAttributesMetaData(this);
         grid_->completeGrid(); // grid generation, to be checked
         if (fileIn_->isClientSide()) fileIn_->readFieldAttributesValues(this);
         grid_->checkElementsAttributes() ;
         grid_->solveDomainAxisBaseRef();
         // probably in future tag grid incomplete if coming from a reading
         instantDataFilter=inputFilter ;
      }  
      else if (hasCouplerIn())
      {
        grid_->checkElementsAttributes() ;
        instantDataFilter=inputFilter ;
      }
      else
      {
        setModelIn() ; // no reference, the field is potentially a source field from model

        grid_->solveElementsRefInheritance() ;
        grid_->completeGrid(); // grid generation, to be checked
        grid_->checkElementsAttributes() ;
        instantDataFilter=inputFilter ;
      }
    }
    
    if (hasFileOut())
    {
      if (fileOut_->isServerSide())
      {
        this->solveServerOperation() ;
      }
    }

    buildWorkflowGraphDone_ = true ;
    workflowEnabled_ = true ;
    return true ;
  }
   
  /*!
   * Connect field to filter to send data to server. A temporal filter is inserted before accordingly to the 
   * output frequency of the file
   * \param gc the garbage collector to use when building the filter graph
   */
  void CField::connectToFileServer(CGarbageCollector& gc)
  {
    // insert temporal filter before sending to files
    fileWriterFilter = std::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this, client));
    // insert temporal filter before sending to files
    getTemporalDataFilter(gc, fileOut_->output_freq)->connectOutput(fileWriterFilter, 0);
  } 

  void CField::connectToCouplerOut(CGarbageCollector& gc)
  {
    // insert temporal filter before sending to files
    fileWriterFilter = std::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this, client));
    instantDataFilter->connectOutput(fileWriterFilter, 0);
  } 

  /*!
   * Compute grid index needed to send grid and data to server
   */
  void CField::computeGridIndexToFileServer(void)
  {
    grid_->computeGridIndexToFileServer(client) ;
  }

  /*!
   * Connect field to a source filter to receive data from model.
   */
  void CField::connectToModelInput(CGarbageCollector& gc)
  {
    const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
    const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

    if (check_if_active.isEmpty()) check_if_active = false; 
    clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, false, true, NoneDu, false, detectMissingValues, defaultValue));
    clientSourceFilter -> connectOutput(inputFilter,0) ;
  } 
 
  /*!
   * Connect field to a source filter to receive data from a client (on server side).
   */
  void CField::connectToClientInput(CGarbageCollector& gc)
  {
    clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc,  grid_, false, false));
    clientSourceFilter -> connectOutput(inputFilter,0) ;
  } 


  /*!
   * Connect field to a source filter to receive data from a server (on client side).
   */
  void CField::connectToServerInput(CGarbageCollector& gc)
  {
    const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
    const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

    checkTimeAttributes();
    serverSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, true, false, freq_offset, true,
                                                                          detectMissingValues, defaultValue));
    //serverSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc,  grid_, false, false));
    serverSourceFilter -> connectOutput(inputFilter,0) ;
  } 

  /*!
   * Connect field to a source filter to receive data from coupler (on client side).
   */
   void CField::connectToCouplerIn(CGarbageCollector& gc)
  {
    CContext* context = CContext::getCurrent();

    if (freq_op.isEmpty()) freq_op.setValue(TimeStep);
    if (freq_offset.isEmpty()) freq_offset.setValue(freq_op.getValue() - TimeStep);

    freq_operation_srv = freq_op ;
    last_operation_srv = context->getCalendar()->getInitDate();
    const CDuration toffset = freq_operation_srv - freq_offset.getValue() - context->getCalendar()->getTimeStep();
    last_operation_srv     = last_operation_srv - toffset;

    clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, false, false, freq_offset, true)) ;
    clientSourceFilter -> connectOutput(inputFilter,0) ;
  } 


  /*!
   * Connect field to a file writer filter to write data in file (on server side).
   */
  void CField::connectToFileWriter(CGarbageCollector& gc)
  {
    fileServerWriterFilter = std::shared_ptr<CFileServerWriterFilter>(new CFileServerWriterFilter(gc, this));
    instantDataFilter->connectOutput(fileServerWriterFilter, 0);
  } 
  

  /*!
   * Connect field to a store filter to output data to model on client Side
   */
  void CField::connectToModelOutput(CGarbageCollector& gc)
  {
    const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
    const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

    storeFilter = std::shared_ptr<CStoreFilter>(new CStoreFilter(gc, CContext::getCurrent(), grid_, detectMissingValues, defaultValue));
    instantDataFilter->connectOutput(storeFilter, 0);
  }

  /*!
   * Transform the grid_path attribut into vector of grid.
   * \return the vector CGrid* containing the list of grid path for tranformation
   */ 
  vector<CGrid*> CField::getGridPath(void)
  {
    std::vector<CGrid*> gridPath;

    if (hasDirectFieldReference() && grid_ != getDirectFieldReference()->grid_)
    {
      if (!grid_path.isEmpty())
      {
        std::string gridId;
        size_t start = 0, end;

        do
        {
          end = grid_path.getValue().find(',', start);
          if (end != std::string::npos)
          {
            gridId = grid_path.getValue().substr(start, end - start);
            start = end + 1;
          }
          else gridId = grid_path.getValue().substr(start);

          if (!CGrid::has(gridId))
            ERROR("void CField::solveTransformedGrid()",
               << "Invalid grid_path, the grid '" << gridId << "' does not exist.");

          gridPath.push_back(CGrid::get(gridId));
        }
        while (end != std::string::npos);
      }
    }
    return gridPath ;
  }

  /*!
   * Constructs the graph filter for the field, enabling or not the data output.
   * This method should not be called more than once with enableOutput equal to true.
   *
   * \param gc the garbage collector to use when building the filter graph
   * \param enableOutput must be true when the field data is to be
   *                     read by the client or/and written to a file
   */
   // ym obselete : to be removed later....
  void CField::buildFilterGraph(CGarbageCollector& gc, bool enableOutput)
  TRY
  {     
    if (!isReferenceSolvedAndTransformed) solveAllEnabledFieldsAndTransform();
    if (!isGridChecked) checkGridOfEnabledFields();

    const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
    const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

    CContext* context = CContext::getCurrent();
    bool hasWriterServer = context->getServiceType()==CServicesManager::OUT_SERVER ;
    bool hasIntermediateServer = context->getServiceType()==CServicesManager::GATHERER ;

    if (hasWriterServer)
    {
      if (!instantDataFilter)
        instantDataFilter = clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, true, false));


      // If the field data is to be read by the client or/and written to a file
      if (enableOutput && !storeFilter && !fileWriterFilter)
      {
        if (getRelFile() && (getRelFile()->mode.isEmpty() || getRelFile()->mode == CFile::mode_attr::write))
        {
          fileServerWriterFilter = std::shared_ptr<CFileServerWriterFilter>(new CFileServerWriterFilter(gc, this));
          instantDataFilter->connectOutput(fileServerWriterFilter, 0);
        }
      }
    }
    else if (hasIntermediateServer)
    {
      if (!instantDataFilter)
        instantDataFilter = clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, false, false));

      // If the field data is to be read by the client or/and written to a file
      if (enableOutput && !storeFilter && !fileWriterFilter)
      {
        if (getRelFile() && (getRelFile()->mode.isEmpty() || getRelFile()->mode == CFile::mode_attr::write))
        {
          fileWriterFilter = std::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this, getRelFile()->getContextClient()));
          instantDataFilter->connectOutput(fileWriterFilter, 0);
        }
      }
    }
    else
    {
      // Start by building a filter which can provide the field's instant data
      if (!instantDataFilter)
      {
        // Check if we have an expression to parse
        if (hasExpression())
        {
          boost::scoped_ptr<IFilterExprNode> expr(parseExpr(getExpression() + '\0'));
          std::shared_ptr<COutputPin> filter = expr->reduce(gc, *this);

          // Check if a spatial transformation is needed
          if (!field_ref.isEmpty())
          {
            CGrid* gridRef = CField::get(field_ref)->grid_;
            if (grid_ && grid_ != gridRef && grid_->hasTransform())
            {
                std::pair<std::shared_ptr<CFilter>, std::shared_ptr<CFilter> > filters = CSpatialTransformFilter::buildFilterGraph(gc, gridRef, grid_, detectMissingValues, defaultValue); 

              filter->connectOutput(filters.first, 0);
              filter = filters.second;
            }
          }

          instantDataFilter = filter;
        }
        // Check if we have a reference on another field
        else if (!field_ref.isEmpty()) instantDataFilter = getFieldReference(gc);
        // Check if the data is to be read from a file
        else if (getRelFile() && !getRelFile()->mode.isEmpty() && getRelFile()->mode == CFile::mode_attr::read)
        {
          checkTimeAttributes();
          instantDataFilter = serverSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, true, false, freq_offset, true,
                                                                                                       detectMissingValues, defaultValue));
        }
        else // The data might be passed from the model
        {
          if (check_if_active.isEmpty()) check_if_active = false; 
          instantDataFilter = clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, false, true, NoneDu, false,
                                                                                                      detectMissingValues, defaultValue));
        }
      }

      // If the field data is to be read by the client or/and written to a file
      if (enableOutput && !storeFilter && !fileWriterFilter)
      {
        if (!read_access.isEmpty() && read_access)
        {
          storeFilter = std::shared_ptr<CStoreFilter>(new CStoreFilter(gc, CContext::getCurrent(), grid_,
                                                                          detectMissingValues, defaultValue));
          instantDataFilter->connectOutput(storeFilter, 0);
        }

        if (getRelFile() && (getRelFile()->mode.isEmpty() || getRelFile()->mode == CFile::mode_attr::write))
        {
          fileWriterFilter = std::shared_ptr<CFileWriterFilter>(new CFileWriterFilter(gc, this, getRelFile()->getContextClient()));
          getTemporalDataFilter(gc, getRelFile()->output_freq)->connectOutput(fileWriterFilter, 0);
        }
      }
    }
  }
  CATCH_DUMP_ATTR

  /*!
   * Returns the filter needed to handle the field reference.
   * This method should only be called when building the filter graph corresponding to the field.
   *
   * \param gc the garbage collector to use
   * \return the output pin corresponding to the field reference
   */
  std::shared_ptr<COutputPin> CField::getFieldReference(CGarbageCollector& gc)
  TRY
  {
    if (instantDataFilter || field_ref.isEmpty())
      ERROR("COutputPin* CField::getFieldReference(CGarbageCollector& gc)",
            "Impossible to get the field reference for a field which has already been parsed or which does not have a field_ref.");

    CField* fieldRef = CField::get(field_ref);
    fieldRef->buildFilterGraph(gc, false);

    std::pair<std::shared_ptr<CFilter>, std::shared_ptr<CFilter> > filters;
    // Check if a spatial transformation is needed
    if (grid_ && grid_ != fieldRef->grid_ && grid_->hasTransform())
    {       
      bool hasMissingValue = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
      double defaultValue  = hasMissingValue ? default_value : (!default_value.isEmpty() ? default_value : 0.0);                                
      filters = CSpatialTransformFilter::buildFilterGraph(gc, fieldRef->grid_, grid_, hasMissingValue, defaultValue);
    }
    else
      filters.first = filters.second = std::shared_ptr<CFilter>(new CPassThroughFilter(gc));

    fieldRef->getInstantDataFilter()->connectOutput(filters.first, 0);

    return filters.second;
  }
  CATCH_DUMP_ATTR

  /*!
   * Returns the filter needed to handle a self reference in the field's expression.
   * If the needed filter does not exist, it is created, otherwise it is reused.
   * This method should only be called when building the filter graph corresponding
   * to the field's expression.
   *
   * \param gc the garbage collector to use
   * \return the output pin corresponding to a self reference
   */

/* old version
  std::shared_ptr<COutputPin> CField::getSelfReference(CGarbageCollector& gc)
  TRY
  {
    if (instantDataFilter || !hasExpression())
      ERROR("COutputPin* CField::getSelfReference(CGarbageCollector& gc)",
            "Impossible to add a self reference to a field which has already been parsed or which does not have an expression.");

    if (!selfReferenceFilter)
    {
      const bool detectMissingValues = (!detect_missing_value.isEmpty() && !default_value.isEmpty() && detect_missing_value == true);
      const double defaultValue  = detectMissingValues ? default_value : (!default_value.isEmpty() ? default_value : 0.0);

      if (file && !file->mode.isEmpty() && file->mode == CFile::mode_attr::read)
      {
        if (!serverSourceFilter)
        {
          checkTimeAttributes();
          serverSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, true, false, freq_offset, true,
                                                              detectMissingValues, defaultValue));
         }

        selfReferenceFilter = serverSourceFilter;
      }
      else if (!field_ref.isEmpty())
      {
        CField* fieldRef = CField::get(field_ref);
        fieldRef->buildFilterGraph(gc, false);
        selfReferenceFilter = fieldRef->getInstantDataFilter();
      }
      else
      {
        if (!clientSourceFilter)
        {
          if (check_if_active.isEmpty()) check_if_active = false;
          clientSourceFilter = std::shared_ptr<CSourceFilter>(new CSourceFilter(gc, grid_, true, true, NoneDu, false,
                                                                                detectMissingValues, defaultValue));
        }

        selfReferenceFilter = clientSourceFilter;
      }
    }

    return selfReferenceFilter;
  }
  CATCH_DUMP_ATTR
*/
  std::shared_ptr<COutputPin> CField::getSelfReference(CGarbageCollector& gc)
  TRY
  {
    return inputFilter ;
  } 
  CATCH_DUMP_ATTR

  /*!
   * Returns the temporal filter corresponding to the field's temporal operation
   * for the specified operation frequency. The filter is created if it does not
   * exist, otherwise it is reused.
   *
   * \param gc the garbage collector to use
   * \param outFreq the operation frequency, i.e. the frequency at which the output data will be computed
   * \return the output pin corresponding to the requested temporal filter
   */
  std::shared_ptr<COutputPin> CField::getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)
  TRY
  {
    std::map<CDuration, std::shared_ptr<COutputPin> >::iterator it = temporalDataFilters.find(outFreq);

    if (it == temporalDataFilters.end())
    {
      if (operation.isEmpty())
        ERROR("void CField::getTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)",
              << "An operation must be defined for field \"" << getId() << "\".");

      checkTimeAttributes(&outFreq);

      const bool detectMissingValues = (!detect_missing_value.isEmpty()  && detect_missing_value == true);
      std::shared_ptr<CTemporalFilter> temporalFilter(new CTemporalFilter(gc, operation,
                                                                             CContext::getCurrent()->getCalendar()->getInitDate(),
                                                                             freq_op, freq_offset, outFreq, detectMissingValues));

      instantDataFilter->connectOutput(temporalFilter, 0);

      it = temporalDataFilters.insert(std::make_pair(outFreq, temporalFilter)).first;
    }

    return it->second;
  }
  CATCH_DUMP_ATTR

  /*!
    * Returns the temporal filter corresponding to the field's temporal operation
    * for the specified operation frequency.
    *
    * \param gc the garbage collector to use
    * \param outFreq the operation frequency, i.e. the frequency at which the output data will be computed
    * \return the output pin corresponding to the requested temporal filter
    */

  std::shared_ptr<COutputPin> CField::getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)
  TRY
  {
    if (instantDataFilter || !hasExpression())
      ERROR("COutputPin* CField::getSelfTemporalDataFilter(CGarbageCollector& gc)",
            "Impossible to add a self reference to a field which has already been parsed or which does not have an expression.");
    
    if (selfTemporalDataFilter) return selfTemporalDataFilter;

    if (hasDirectFieldReference())
    {
      CField* fieldRef=getDirectFieldReference();
      return fieldRef->getTemporalDataFilter(gc, outFreq) ;
    }
    else
    {
      if (selfTemporalDataFilter) return selfTemporalDataFilter ;

      if (operation.isEmpty())
        ERROR("void CField::getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)",
              << "An operation must be defined for field \"" << getId() << "\".");

      checkTimeAttributes(&outFreq); //bof

      const bool detectMissingValues = (!detect_missing_value.isEmpty() && detect_missing_value == true);
      selfTemporalDataFilter = std::shared_ptr<CTemporalFilter>(new CTemporalFilter(gc, operation,
                                                                CContext::getCurrent()->getCalendar()->getInitDate(),
                                                                freq_op, freq_offset, outFreq, detectMissingValues));

      inputFilter->connectOutput(selfTemporalDataFilter, 0);
      return selfTemporalDataFilter ;
    }
  }
  CATCH_DUMP_ATTR

/* old version   
  std::shared_ptr<COutputPin> CField::getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)
  TRY
  {
    if (instantDataFilter || !hasExpression())
      ERROR("COutputPin* CField::getSelfTemporalDataFilter(CGarbageCollector& gc)",
            "Impossible to add a self reference to a field which has already been parsed or which does not have an expression.");

    if (!selfReferenceFilter) getSelfReference(gc) ;

    if (serverSourceFilter || clientSourceFilter)
    {
      if (operation.isEmpty())
        ERROR("void CField::getSelfTemporalDataFilter(CGarbageCollector& gc, CDuration outFreq)",
              << "An operation must be defined for field \"" << getId() << "\".");

      checkTimeAttributes(&outFreq);

      const bool detectMissingValues = (!detect_missing_value.isEmpty() && detect_missing_value == true);
      std::shared_ptr<CTemporalFilter> temporalFilter(new CTemporalFilter(gc, operation,
                                                                          CContext::getCurrent()->getCalendar()->getInitDate(),
                                                                          freq_op, freq_offset, outFreq, detectMissingValues));

      selfReferenceFilter->connectOutput(temporalFilter, 0);
      return temporalFilter ;
    }
    else if (!field_ref.isEmpty())
    {
      CField* fieldRef = CField::get(field_ref);
      fieldRef->buildFilterGraph(gc, false); 
      return fieldRef->getTemporalDataFilter(gc, outFreq) ;
    }
  }
  CATCH_DUMP_ATTR
*/

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
  TRY
  {
    if (grid_!=nullptr) return ; // already done

    if (grid_ref.isEmpty() && domain_ref.isEmpty() && axis_ref.isEmpty() && scalar_ref.isEmpty())
    {
      ERROR("CField::solveGridReference(void)",
            << "A grid must be defined for field '" << getFieldOutputName() << "' .");
    }
    else if (!grid_ref.isEmpty() && (!domain_ref.isEmpty() || !axis_ref.isEmpty() || !scalar_ref.isEmpty()))
    {
      ERROR("CField::solveGridReference(void)",
            << "Field '" << getFieldOutputName() << "' has both a grid and a domain/axis/scalar." << std::endl
            << "Please define either 'grid_ref' or 'domain_ref'/'axis_ref'/'scalar_ref'.");
    }

    if (grid_ref.isEmpty())
    {
      std::vector<CDomain*> vecDom;
      std::vector<CAxis*> vecAxis;
      std::vector<CScalar*> vecScalar;
      std::vector<int> axisDomainOrderTmp;

      std::vector<CDomain*> vecDomRef;
      std::vector<CAxis*> vecAxisRef;
      std::vector<CScalar*> vecScalarRef;
        
      if (!domain_ref.isEmpty())
      {
        StdString tmp = domain_ref.getValue();
        if (CDomain::has(domain_ref))
        {
          vecDom.push_back(CDomain::get(domain_ref));
          vecDomRef.push_back(CDomain::createDomain());
          vecDomRef.back()->domain_ref=domain_ref;
          axisDomainOrderTmp.push_back(2);
        }
        else  ERROR("CField::solveGridReference(void)",
                    << "Invalid reference to domain '" << domain_ref.getValue() << "'.");
      }

      if (!axis_ref.isEmpty())
      {
        if (CAxis::has(axis_ref))
        {
          vecAxis.push_back(CAxis::get(axis_ref));
          vecAxisRef.push_back(CAxis::createAxis());
          vecAxisRef.back()->axis_ref=axis_ref;
          axisDomainOrderTmp.push_back(1);
        }
        else  ERROR("CField::solveGridReference(void)",
                    << "Invalid reference to axis '" << axis_ref.getValue() << "'.");
      }

      if (!scalar_ref.isEmpty())
      {
        if (CScalar::has(scalar_ref))
        {
          vecScalar.push_back(CScalar::get(scalar_ref));
          vecScalarRef.push_back(CScalar::createScalar());
          vecScalarRef.back()->scalar_ref=scalar_ref;
          axisDomainOrderTmp.push_back(0);
        }
        else ERROR("CField::solveGridReference(void)",
                   << "Invalid reference to scalar '" << scalar_ref.getValue() << "'.");
      }
        
      CArray<int,1> axisDomainOrder(axisDomainOrderTmp.size());
      for (int idx = 0; idx < axisDomainOrderTmp.size(); ++idx)
      {
        axisDomainOrder(idx) = axisDomainOrderTmp[idx];
      }

      // Warning: the gridId shouldn't be set as the grid_ref since it could be inherited
      StdString gridId = CGrid::generateId(vecDom, vecAxis, vecScalar,axisDomainOrder);
      if (CGrid::has(gridId)) this->grid_ = CGrid::get(gridId);
      else  this->grid_ = CGrid::createGrid(gridId, vecDomRef, vecAxisRef, vecScalarRef,axisDomainOrder);
    }
    else
    {
      if (CGrid::has(grid_ref)) this->grid_ = CGrid::get(grid_ref);
      else  ERROR("CField::solveGridReference(void)",
                   << "Invalid reference to grid '" << grid_ref.getValue() << "'.");
    }
  }
  CATCH_DUMP_ATTR

  void CField::solveGridDomainAxisRef(bool checkAtt)
  TRY
  {
    grid_->solveDomainAxisRef(checkAtt);
  }
  CATCH_DUMP_ATTR

  void CField::solveCheckMaskIndex(bool doSendingIndex)
  TRY
  {
    grid_->checkMaskIndex(doSendingIndex);
  }
  CATCH_DUMP_ATTR

  void CField::solveTransformedGrid()
  TRY
  {
    if (grid_ && !grid_->isTransformed() && hasDirectFieldReference() && grid_ != getDirectFieldReference()->grid_)
    {
      std::vector<CGrid*> grids;
      // Source grid
      grids.push_back(getDirectFieldReference()->grid_);
      auto gridPath = getGridPath() ;
      grids.insert(grids.begin(), gridPath.begin(), gridPath.end());

      for (size_t i = 0, count = grids.size() - 1; i < count; ++i)
      {
        CGrid *gridSrc  = grids[i];
        CGrid *gridDest = grids[i + 1];
        if (!gridDest->isTransformed()) gridDest->transformGrid(gridSrc);
      }
    }
    else if (grid_ && grid_->hasTransform() && !grid_->isTransformed())
    {
      // Temporarily deactivate the self-transformation of grid
      // grid_->transformGrid(grid_);
    }
  }
  CATCH_DUMP_ATTR

  void CField::solveGenerateGrid()
  TRY
  {
    if (grid_ && !grid_->isTransformed() && hasDirectFieldReference() && grid_ != getDirectFieldReference()->grid_)
      grid_->completeGrid(getDirectFieldReference()->grid_);
    else grid_->completeGrid();
  }
  CATCH_DUMP_ATTR

  void CField::solveGridDomainAxisBaseRef()
  TRY
  {
    grid_->solveDomainAxisRef(false);
    grid_->solveDomainAxisBaseRef();
  }
  CATCH_DUMP_ATTR

  ///-------------------------------------------------------------------

  template <>
  void CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)
  TRY
  {
    if (this->group_ref.isEmpty()) return;
    StdString gref = this->group_ref.getValue();

    if (!CFieldGroup::has(gref))
      ERROR("CGroupTemplate<CField, CFieldGroup, CFieldAttributes>::solveRefInheritance(void)",
         << "[ gref = " << gref << "]"
         << " invalid group name !");

    CFieldGroup* group = CFieldGroup::get(gref);
    CFieldGroup* owner = CFieldGroup::get(boost::polymorphic_downcast<CFieldGroup*>(this));
    owner->setAttributes(group); // inherite of attributes of group reference
      
    std::vector<CField*> allChildren  = group->getAllChildren();
    std::vector<CField*>::iterator it = allChildren.begin(), end = allChildren.end();

    for (; it != end; it++)
    {
      CField* child = *it;
     if (child->hasId()) owner->createChild()->field_ref.setValue(child->getId());
    }
  }
  CATCH_DUMP_ATTR

  void CField::scaleFactorAddOffset(double scaleFactor, double addOffset)
  TRY
  {
    recvDataSrv = (recvDataSrv - addOffset) / scaleFactor;
  }
  CATCH_DUMP_ATTR

  void CField::invertScaleFactorAddOffset(double scaleFactor, double addOffset)
  TRY
  {
    recvDataSrv = recvDataSrv * scaleFactor + addOffset;
  }
  CATCH_DUMP_ATTR

  void CField::outputField(CArray<double,1>& fieldOut)
  TRY
  { 
    CArray<size_t,1>& outIndexClient = grid_->localIndexToWriteOnClient_;
    CArray<size_t,1>& outIndexServer = grid_->localIndexToWriteOnServer_;
    for (size_t idx = 0; idx < outIndexServer.numElements(); ++idx)
    {
      fieldOut(outIndexServer(idx)) = recvDataSrv(outIndexClient(idx));
    }
  }
  CATCH_DUMP_ATTR

  void CField::inputField(CArray<double,1>& fieldIn)
  TRY
  {
    CArray<size_t,1>& outIndexClient = grid_->localIndexToWriteOnClient_;
    CArray<size_t,1>& outIndexServer = grid_->localIndexToWriteOnServer_;
    for (size_t idx = 0; idx < outIndexServer.numElements(); ++idx)
    {
      recvDataSrv(outIndexClient(idx)) = fieldIn(outIndexServer(idx));
    }
  }
  CATCH_DUMP_ATTR

 void CField::outputCompressedField(CArray<double,1>& fieldOut)
 TRY
 {
    CArray<size_t,1>& outIndexClient = grid_->localIndexToWriteOnClient_;
    CArray<size_t,1>& outIndexServer = grid_->localIndexToWriteOnServer_;
    for (size_t idx = 0; idx < outIndexServer.numElements(); ++idx)
    {
      fieldOut((idx)) = recvDataSrv(outIndexClient(idx));
    }
  }
  CATCH_DUMP_ATTR

  ///-------------------------------------------------------------------

  void CField::parse(xml::CXMLNode& node)
  TRY
  {
    string newContent ;
    SuperClass::parse(node);
    if (node.goToChildElement())
    {
      do
      {
        if (node.getElementName() == "variable" || node.getElementName() == "variable_group") this->getVirtualVariableGroup()->parseChild(node);
        else if (node.getElementName() == "expr") if (node.getContent(newContent)) content+=newContent ;
      } while (node.goToNextElement());
      node.goToParentElement();
    }
    if (node.getContent(newContent)) content=newContent ;
  }
  CATCH_DUMP_ATTR

 /*!
   This function retrieves Id of corresponding domain_ref and axis_ref (if any)
   of a field. In some cases, only domain exists but axis doesn't
   \return pair of Domain and Axis id
  */
  const std::vector<StdString>& CField::getRefDomainAxisIds()
  TRY
  {
    CGrid* cgPtr = getRelGrid();
    if (NULL != cgPtr)
    {
      std::vector<StdString>::iterator it;
      if (!domain_ref.isEmpty())
      {
        std::vector<StdString> domainList = cgPtr->getDomainList();
        it = std::find(domainList.begin(), domainList.end(), domain_ref.getValue());
        if (domainList.end() != it) domAxisScalarIds_[0] = *it;
      }

      if (!axis_ref.isEmpty())
      {
        std::vector<StdString> axisList = cgPtr->getAxisList();
        it = std::find(axisList.begin(), axisList.end(), axis_ref.getValue());
        if (axisList.end() != it) domAxisScalarIds_[1] = *it;
      }

      if (!scalar_ref.isEmpty())
      {
        std::vector<StdString> scalarList = cgPtr->getScalarList();
        it = std::find(scalarList.begin(), scalarList.end(), scalar_ref.getValue());
        if (scalarList.end() != it) domAxisScalarIds_[2] = *it;
      }
    }
    return (domAxisScalarIds_);
  }
  CATCH_DUMP_ATTR

  CVariable* CField::addVariable(const string& id)
  TRY
  {
    return vVariableGroup->createChild(id);
  }
  CATCH

  CVariableGroup* CField::addVariableGroup(const string& id)
  TRY
  {
    return vVariableGroup->createChildGroup(id);
  }
  CATCH

  void CField::setContextClient(CContextClient* contextClient)
  TRY
  {
    CContext* context = CContext::getCurrent();
    client = contextClient;
  
    // A grid is sent by a client (both for read or write) or by primary server (write only)
    if (context->getServiceType()==CServicesManager::GATHERER)
    {
      if (getRelFile()->mode.isEmpty() || (!getRelFile()->mode.isEmpty() && getRelFile()->mode == CFile::mode_attr::write))
        grid_->setContextClient(contextClient);
    }
    else if (context->getServiceType()==CServicesManager::CLIENT)
      grid_->setContextClient(contextClient);
  }
  CATCH_DUMP_ATTR

  void CField::sendCloseDefinition(void)
  {
    CContext::getCurrent()->sendCloseDefinition(client) ;
  }

  void CField::sendFieldToFileServer(void)
  {
    CContext::getCurrent()->sendContextToFileServer(client);
    getRelFile()->sendFileToFileServer(client);
    grid_->sendGridToFileServer(client);
    this->sendAllAttributesToServer(client);
    this->sendAddAllVariables(client);
  }

  void CField::sendFieldToCouplerOut(void)
  {
    if (sendFieldToCouplerOut_done_) return ;
    else sendFieldToCouplerOut_done_=true ;
    grid_->sendGridToCouplerOut(client, this->getId());
    this->sendGridCompleted();

  }
  
  void CField::makeGridAliasForCoupling(void) 
  { 
    grid_->makeAliasForCoupling(this->getId()); 
  }

 //! Client side: Send a message  announcing that the grid definition has been received from a coupling context
   void CField::sendGridCompleted(void)
   TRY
   {
      CEventClient event(getType(),EVENT_ID_GRID_COMPLETED);

      if (client->isServerLeader())
      {
        CMessage msg;
        msg<<this->getId();
        for (auto& rank : client->getRanksServerLeader()) event.push(rank,1,msg);
        client->sendEvent(event);
      }
      else client->sendEvent(event);
   }
   CATCH_DUMP_ATTR

   //! Server side: Receive a message announcing that the grid definition has been received from a coupling context
   void CField::recvGridCompleted(CEventServer& event)
   TRY
   {
      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvGridCompleted(*buffer);
   }
   CATCH

   //! Server side: Receive a message  message  announcing that the grid definition has been received from a coupling context
   void CField::recvGridCompleted(CBufferIn& buffer)
   TRY
   {
      setGridCompleted() ;
   }
   CATCH_DUMP_ATTR





  void CField::sendFieldToInputFileServer(void)
  {
    CContext::getCurrent()->sendContextToFileServer(client);
    getRelFile()->sendFileToFileServer(client);
    grid_->sendGridToFileServer(client);
    read_access=true ; // not the best solution, but on server side, the field must be a starting point of the workflow
                       // must be replace by a better solution when implementing filters for reading and send to client
                       // on server side
    this->sendAllAttributesToServer(client);
    this->sendAddAllVariables(client);
  }

  void CField::sendAddAllVariables(CContextClient* client)
  TRY
  {
    std::vector<CVariable*> allVar = getAllVariables();
    std::vector<CVariable*>::const_iterator it = allVar.begin();
    std::vector<CVariable*>::const_iterator itE = allVar.end();

    for (; it != itE; ++it)
    {
      this->sendAddVariable((*it)->getId(), client);
      (*it)->sendAllAttributesToServer(client);
      (*it)->sendValue(client);
    }
  }
  CATCH_DUMP_ATTR

  /*!
   * Send all Attributes to server. This method is overloaded, since only grid_ref attribute
   * must be sent to server and not domain_ref/axis_ref/scalar_ref. 
   */
    
  void CField::sendAllAttributesToServer(CContextClient* client)
  TRY
  {
    if (grid_ref.isEmpty())
    {
      grid_ref=grid_->getId() ;
      SuperClass::sendAllAttributesToServer(client) ;
      domain_ref.reset() ;
      axis_ref.reset() ;
      scalar_ref.reset() ;
      grid_ref.reset();
    }
    else SuperClass::sendAllAttributesToServer(client) ;
  }
  CATCH_DUMP_ATTR
    
  void CField::sendAddVariable(const string& id, CContextClient* client)
  TRY
  {
    sendAddItem(id, (int)EVENT_ID_ADD_VARIABLE, client);
  }
  CATCH_DUMP_ATTR

  void CField::sendAddVariableGroup(const string& id, CContextClient* client)
  TRY
  {
    sendAddItem(id, (int)EVENT_ID_ADD_VARIABLE_GROUP, client);
  }
  CATCH_DUMP_ATTR

  void CField::recvAddVariable(CEventServer& event)
  TRY
  {
    CBufferIn* buffer = event.subEvents.begin()->buffer;
    string id;
    *buffer >> id;
    get(id)->recvAddVariable(*buffer);
  }
  CATCH

  void CField::recvAddVariable(CBufferIn& buffer)
  TRY
  {
    string id;
    buffer >> id;
    addVariable(id);
  }
  CATCH_DUMP_ATTR

  void CField::recvAddVariableGroup(CEventServer& event)
  TRY
  {
    CBufferIn* buffer = event.subEvents.begin()->buffer;
    string id;
    *buffer >> id;
    get(id)->recvAddVariableGroup(*buffer);
  }
  CATCH

  void CField::recvAddVariableGroup(CBufferIn& buffer)
  TRY
  {
    string id;
    buffer >> id;
    addVariableGroup(id);
  }
  CATCH_DUMP_ATTR

  /*!
   * Check on freq_off and freq_op attributes.
   */
  void CField::checkTimeAttributes(CDuration* freqOp)
  TRY
  {
    if (hasFileIn() && !(operation.getValue() == "instant" || operation.getValue() == "once") )     
      ERROR("void CField::checkTimeAttributes(void)",
         << "Unsupported operation for field '" << getFieldOutputName() << "'." << std::endl
         << "Currently only \"instant\" is supported for fields read from file.")

    if (freq_op.isEmpty())
    {
      if (operation.getValue() == "instant")
      {
        if (hasFileIn() || hasFileOut()) freq_op.setValue(getRelFile()->output_freq.getValue());
        else freq_op=*freqOp ;
      }
      else freq_op.setValue(TimeStep);
    }
    if (freq_offset.isEmpty()) freq_offset.setValue(hasFileIn() ? NoneDu : (freq_op.getValue() - TimeStep));
  }
  CATCH_DUMP_ATTR

  /*!
   * Returns string arithmetic expression associated to the field.
   * \return if content is defined return content string, otherwise, if "expr" attribute is defined, return expr string.
   */
  const string& CField::getExpression(void)
  TRY
  {
    if (!expr.isEmpty() && content.empty())
    {
      content = expr;
      expr.reset();
    }

    return content;
  }
  CATCH_DUMP_ATTR

  bool CField::hasExpression(void) const
  TRY
  {
    return (!expr.isEmpty() || !content.empty());
  }
  CATCH

  bool CField::hasGridMask(void) const
  TRY
  {
    return (this->grid_->hasMask());
  }
  CATCH

  DEFINE_REF_FUNC(Field,field)
} // namespace xios
