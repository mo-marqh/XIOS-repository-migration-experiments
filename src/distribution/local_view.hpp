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
    CLocalView(shared_ptr<CLocalElement> parent, CElementView::type type, const CArray<int,1>& indexView) ;
    CLocalView(shared_ptr<CLocalElement> parent, CElementView::type type, const CArray<bool,1>& maskView) ;

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
      if(pos==0) globalIndexView.resize(pos) ;
      else globalIndexView.resizeAndPreserve(pos) ;
    }    

    void getGlobalIndex(vector<size_t>& globalIndex, size_t sliceIndex, size_t* sliceSize, shared_ptr<CLocalView>* localView, int pos)
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
    void sendRemoteElement(shared_ptr<CRemoteConnector> connector, CContextClient* client, CEventClient& event, const CMessage& messageHeader) ;
   
    CArray<size_t,1>& globalIndex_ ;
    CArray<int,1>& index_ ;
    int& size_ ;        /** The number of index composing the view, ie the size of the corresponding data, ie the size of index_ */
    int& localRank_ ;
    int& localSize_ ;   /** The local size of the element, ie the size of globalIndex_ */
  } ;

}

#endif