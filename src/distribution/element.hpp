#ifndef __ELEMENT_HPP__
#define __ELEMENT_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"
#include "element_view.hpp"
#include "exception.hpp"
#include "context_client.hpp"
#include "context_server.hpp"

namespace xios
{
  class CDistributedView ;
  class CLocalView;
  class CLocalConnector ;

  class CDistributedElement
  {

  protected:
    std::map<int, CArray<size_t,1>> globalIndex_ ;
    std::map<int, int> localSize_ ;
    size_t globalSize_ ;
    std::vector<CDistributedView*> views_= std::vector<CDistributedView*>(CElementView::numViewType_) ;
    CDistributedElement(void) {} ;
    
  public:
    CDistributedElement(int globalSize, const map<int, CArray<size_t,1>>& globalIndex) ;
    CDistributedElement(CEventServer& event) ;
    void addFullView(void) ;
    void sendToServer(CContextClient* client, CEventClient& event, const CMessage& messageHeader) ;
    void recvFromClient(CEventServer& event) ;
    size_t getGlobalSize(void) { return globalSize_;}

    CDistributedView* getView(CElementView::type type) 
    { 
      if (views_[(size_t)type]==nullptr) { ERROR("CDistributedElement::getView(CElementView::type type)",<<"View is not initialized");} 
      else return views_[(size_t)type] ; 
    }

    void addView(CElementView::type type, std::map<int, CArray<int,1>>& indexView) ;
    void addView(CElementView::type type, std::map<int, CArray<bool,1>>& maskView) ;
    void sendToServer(CEventClient& event, const CMessage& messageHeader) ;

    friend class CDistributedView ;
  } ;


  class CLocalElement : public CDistributedElement
  {
    // keep local connector inside    
      std::map<pair<CElementView::type,CElementView::type>, CLocalConnector*> connectors_  ;
 
    public: 
      CLocalElement(int localRank, size_t globalSize, CArray<size_t,1>& globalIndex) ;
      CLocalElement(int localRank, CEventServer& event) ;
      void recvFromClient(int localRank, CEventServer& event) ;
      const CArray<size_t,1>& getGlobalIndex(void) { return globalIndex_ ;}
      void addView(CElementView::type type, CArray<int,1>& indexView) ;
      void addView(CElementView::type type, CArray<bool,1>& maskView) ;
      void addFullView(void) ;
      CLocalView* getView(CElementView::type type) 
      { 
        if (views_[(size_t)type]==nullptr) { ERROR("CLocalElement::getView(CElementView::type type)",<<"View is not initialized");} 
        else return (CLocalView*) views_[(size_t)type] ; 
      }
      
      CLocalConnector* getConnector(CElementView::type srcType, CElementView::type dstType) ;

    private :
      int localRank_;
      CArray<size_t,1>& globalIndex_ ;
      int& localSize_ ;
    
    friend class CLocalView ;
  } ;

}

#endif

