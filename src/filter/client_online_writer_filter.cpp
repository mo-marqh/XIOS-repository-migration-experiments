#include "context.hpp"

#include "grid_redistribute_filter.hpp"
#include "client_online_writer_filter.hpp"
#include "field.hpp"
#include "file.hpp"


namespace xios
{


CClientOnlineWriterFilter::CClientOnlineWriterFilter(CGarbageCollector& gc, CField* field)
    : CFilter(gc, 1, this)
  {
    CField* fieldOut ;
    redistributeFilter_ = std::shared_ptr<CGridRedistributeFilter>(new CGridRedistributeFilter(gc, field, fieldOut));
    fieldOut->setVirtualVariableGroup( field->getVirtualVariableGroup() );
    fieldOut->setFileOut(field->getFileOut());
    field->getFileOut()->replaceEnabledFields(field, fieldOut) ;
    fieldOut->solveServerOperation() ; // might not be called, create a new time functor.... find a better solution later
    
    fileWriterStoreFilter_ = std::shared_ptr<CFileWriterStoreFilter>(new CFileWriterStoreFilter(gc, fieldOut));
    redistributeFilter_->connectOutput(fileWriterStoreFilter_, 0);
    connectOutput(redistributeFilter_,0) ;
  }
  
  CDataPacketPtr CClientOnlineWriterFilter::apply(std::vector<CDataPacketPtr> data)
  {
    return data[0];
  }

}
