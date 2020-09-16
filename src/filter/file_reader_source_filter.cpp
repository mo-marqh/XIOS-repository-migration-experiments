#include "file_reader_source_filter.hpp"
#include "grid.hpp"
#include "exception.hpp"
#include "calendar_util.hpp"
#include "context.hpp"
#include "field.hpp"
#include "file.hpp"
#include "context.hpp"


namespace xios
{
  CFileReaderSourceFilter::CFileReaderSourceFilter(CGarbageCollector& gc, CField* field)
    : COutputPin(gc)
  {
    field_ = field ;
    grid_ = field->getGrid() ;
    file_ = field->getFileIn() ;
    if (!file_->cyclic.isEmpty()) isCyclic_ = file_->cyclic ;
    if (!field_->scale_factor.isEmpty()) { hasScaleFactor_=true ; scaleFactor_ = field_->scale_factor ; }
    if (!field_->add_offset.isEmpty()) { hasAddOffset_=true ; addOffset_ = field_->add_offset ; }
  }

  void CFileReaderSourceFilter::streamData()
  {
    Time timeStamp ;
    CDataPacketPtr packet(new CDataPacket);
    packet->date = CContext::getCurrent()->getCalendar()->getCurrentDate();
    packet->timestamp = timeStamp;
    packet->status = CDataPacket::NO_ERROR;

    if (!isInitialized_)  initialize() ;
    CField::EReadField readState = CField::RF_DATA;
    if ( nStepMax_==0 || (nStep_ >= nStepMax_ && !isCyclic_)) readState = CField::RF_EOF;
  
    if (CField::RF_EOF != readState)
    {
      if (!file_->isEmptyZone()) readData(packet->data) ;
      else readState = CField::RF_NODATA;
    }
    nStep_++ ;
    
    if (readState == CField::RF_DATA) packet->status = CDataPacket::NO_ERROR;
    else packet->status = CDataPacket::END_OF_STREAM;
    
    info(20)<<"Read data from file : FieldId "<<field_->getId()<<"  nStep "<<nStep_<<"  date : "<<packet->date<<endl ;
           
    onOutputReady(packet);
  }

  void CFileReaderSourceFilter::initialize()
  {
    CContext* context = CContext::getCurrent();
    file_->initRead();
    if (!file_->isEmptyZone())
    {      
      file_->checkReadFile();
      nStepMax_ = file_->getDataInput()->getFieldNbRecords(field_);
      nStep_ = 0 ;
    }
    MPI_Allreduce(MPI_IN_PLACE, &nStepMax_, 1, MPI_INT, MPI_MAX, context->getIntraComm());
    isInitialized_=true;
  }

  void CFileReaderSourceFilter::readData(CArray<double,1>& data)
  {
    CGridLocalConnector*  connector = grid_->getFullToWorkflowConnector() ;
    CArray<double,1> dataIn(connector->getSrcSize()) ;
    file_->getDataInput()->readFieldData(field_, nStep_%nStepMax_, dataIn);
    data.resize(connector->getDstSize()) ;
    connector->transfer(dataIn, data) ; 

    if (hasScaleFactor_ || hasAddOffset_) data = data * scaleFactor_ + addOffset_; // possibility of optimization
  }
 
  
} // namespace xios
