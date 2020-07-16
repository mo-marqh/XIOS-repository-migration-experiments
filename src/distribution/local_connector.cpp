#include "local_connector.hpp"


namespace xios
{
    void CLocalConnector::computeConnector(void)
  {
    int srcLocalSize=srcView_->getLocalSize() ;
    const CArray<int,1>& srcIndex = srcView_->getIndex() ;
    int srcSize = srcIndex.numElements() ;

    int dstLocalSize=dstView_->getLocalSize() ;
    const CArray<int,1>& dstIndex = dstView_->getIndex() ;
    int dstSize = dstIndex.numElements() ;

    CArray<int,1> local(srcLocalSize) ;
    local = -1 ;
    mask_.resize(dstSize) ;

    for(int i=0;i<srcSize;i++)
    {
      if (srcIndex(i)>=0 && srcIndex(i) < srcLocalSize) local(srcIndex(i)) = i ;
    }

    int connectorSize=0 ;
    for(int i=0;i<dstSize;i++)
    {
      if (dstIndex(i)>=0 && dstIndex(i) < dstLocalSize && local(dstIndex(i))!=-1)
      { 
        mask_[i]=true ;
        connectorSize++ ;
      }
      else mask_[i]=false ;
    }
    
    connector_.resize(connectorSize) ;
    connectorSize=0 ;
    for(int i=0;i<dstSize;i++)
    {
      if (mask_[i])
      {
        connector_[connectorSize] = local(dstIndex(i)) ;
        connectorSize++ ;
      }
    }
  }



}