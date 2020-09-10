#ifndef __LOCAL_VIEW_HPP__
#define __LOCAL_VIEW_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "distributed_view.hpp"
#include "context_client.hpp"

namespace xios
{

  class CLocalElement ;
  class CRemoteConnector ;

  class CLocalView : public CDistributedView
  {
    public:
    CLocalView(CLocalElement* parent, CElementView::type type, const CArray<int,1>& indexView) ;
    CLocalView(CLocalElement* parent, CElementView::type type, const CArray<bool,1>& maskView) ;

    const CArray<int,1>& getIndex(void) { return index_ ;}
    const CArray<size_t,1>& getGlobalIndex(void) { return globalIndex_ ;}
    
    void getGlobalIndexView(CArray<size_t,1>& globalIndexView)
    {
      globalIndexView.resize(size_) ;
      int pos=0 ;
      for(int i=0 ; i<size_ ; i++)
      {
        if (index_(i)>=0 && index_(i)<localSize_) 
        {
          globalIndexView(i) = globalIndex_(index_(i)) ;
          pos++ ;
        }
      }
      globalIndexView.resizeAndPreserve(pos) ;
    }    

    void getGlobalIndex(vector<size_t>& globalIndex, size_t sliceIndex, size_t* sliceSize, CLocalView** localView, int pos)
    {
      if (pos==0)
      {
        for(int i=0;i<size_;i++)
          if (index_(i)>=0 && index_(i)<localSize_) globalIndex.push_back(sliceIndex + globalIndex_(index_(i))) ;
      }
      else 
      {
        for(int i=0;i<size_;i++)
          if (index_(i)>=0 && index_(i)<localSize_) 
            localView[pos-1]->getGlobalIndex(globalIndex, sliceIndex + globalIndex_(index_(i))*sliceSize[pos], sliceSize, localView, pos-1) ;
      }
    }


    int getLocalSize(void) {return localSize_ ;}
    int getSize(void) {return size_;} 
    void sendRemoteElement(CRemoteConnector& connector, CContextClient* client, CEventClient& event, const CMessage& messageHeader) ;
   
    CArray<size_t,1>& globalIndex_ ;
    CArray<int,1>& index_ ;
    int& size_ ;
    int& localRank_ ;
    int& localSize_ ;
  } ;

}

#endif