
#include "grid.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include <iostream>
#include "xmlioserver_spl.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "array_new.hpp"

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , withAxis(false), isChecked(false), isDomainAxisChecked(false), axis(), domain()
      , storeIndex(1), out_i_index(1), out_j_index(1), out_l_index(1), isDomConServerComputed_(false)
      , vDomainGroup_(), vAxisGroup_(), axisList_(), isAxisListSet(false), isDomListSet(false), clientDistribution_(0), isIndexSent(false)
      , serverDistribution_(0), serverDistributionDescription_(0), clientServerMap_()
   {
     setVirtualDomainGroup();
     setVirtualAxisGroup();
   }

   CGrid::CGrid(const StdString & id)
      : CObjectTemplate<CGrid>(id), CGridAttributes()
      , withAxis(false), isChecked(false), isDomainAxisChecked(false), axis(), domain()
      , storeIndex(1), out_i_index(1), out_j_index(1), out_l_index(1), isDomConServerComputed_(false)
      , vDomainGroup_(), vAxisGroup_(), axisList_(), isAxisListSet(false), isDomListSet(false), clientDistribution_(0), isIndexSent(false)
      , serverDistribution_(0), serverDistributionDescription_(0), clientServerMap_()
   {
     setVirtualDomainGroup();
     setVirtualAxisGroup();
   }

   CGrid::~CGrid(void)
   {
 //     this->axis.reset() ;
//      this->domain.reset() ;
    deque< CArray<int, 1>* >::iterator it ;

    for(deque< CArray<int,1>* >::iterator it=storeIndex.begin(); it!=storeIndex.end();it++)  delete *it ;
    for(deque< CArray<int,1>* >::iterator it=out_i_index.begin();it!=out_i_index.end();it++) delete *it ;
    for(deque< CArray<int,1>* >::iterator it=out_j_index.begin();it!=out_j_index.end();it++) delete *it ;
    for(deque< CArray<int,1>* >::iterator it=out_l_index.begin();it!=out_l_index.end();it++) delete *it ;

    for(map<int,CArray<int,1>* >::iterator it=out_i_fromClient.begin();it!=out_i_fromClient.end();it++) delete it->second ;
    for(map<int,CArray<int,1>* >::iterator it=out_j_fromClient.begin();it!=out_j_fromClient.end();it++) delete it->second ;
    for(map<int,CArray<int,1>* >::iterator it=out_l_fromClient.begin();it!=out_l_fromClient.end();it++) delete it->second ;

    for(map<int,CArray<size_t,1>* >::iterator it=outIndexFromClient.begin();it!=outIndexFromClient.end();++it) delete (it->second);

    if (0 != clientDistribution_) delete clientDistribution_;
    if (0 != serverDistribution_) delete serverDistribution_;
    if (0 != serverDistributionDescription_) delete serverDistributionDescription_;

   }

   ///---------------------------------------------------------------

   StdString CGrid::GetName(void)    { return (StdString("grid")); }
   StdString CGrid::GetDefName(void) { return (CGrid::GetName()); }
   ENodeType CGrid::GetType(void)    { return (eGrid); }

   //----------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getStoreIndex(void) const
   {
      return (this->storeIndex );
   }

   //---------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getOutIIndex(void)  const
   {
      return (this->out_i_index );
   }

   //---------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getOutJIndex(void)  const
   {
      return (this->out_j_index );
   }

   //---------------------------------------------------------------

   const std::deque< CArray<int,1>* > & CGrid::getOutLIndex(void)  const
   {
      return (this->out_l_index );
   }

   //---------------------------------------------------------------

   const CAxis*   CGrid::getRelAxis  (void) const
   {
      return (this->axis );
   }

   //---------------------------------------------------------------

   const CDomain* CGrid::getRelDomain(void) const
   {
      return (this->domain );
   }

   //---------------------------------------------------------------

   bool CGrid::hasAxis(void) const
   {
      return (this->withAxis);
   }

   //---------------------------------------------------------------

   StdSize CGrid::getDimension(void) const
   {
      return ((this->withAxis)?3:2);
   }

   //---------------------------------------------------------------

/*
   std::vector<StdSize> CGrid::getLocalShape(void) const
   {
      std::vector<StdSize> retvalue;
      retvalue.push_back(domain->zoom_ni_loc.getValue());
      retvalue.push_back(domain->zoom_nj_loc.getValue());
      if (this->withAxis)
         retvalue.push_back(this->axis->zoom_size.getValue());
      return (retvalue);
   }
*/
   //---------------------------------------------------------------

/*
   StdSize CGrid::getLocalSize(void) const
   {
      StdSize retvalue = 1;
      std::vector<StdSize> shape_ = this->getLocalShape();
      for (StdSize s = 0; s < shape_.size(); s++)
         retvalue *= shape_[s];
      return (retvalue);
   }
*/
   //---------------------------------------------------------------
/*
   std::vector<StdSize> CGrid::getGlobalShape(void) const
   {
      std::vector<StdSize> retvalue;
      retvalue.push_back(domain->ni.getValue());
      retvalue.push_back(domain->nj.getValue());
      if (this->withAxis)
         retvalue.push_back(this->axis->size.getValue());
      return (retvalue);
   }
*/
   //---------------------------------------------------------------

/*
   StdSize CGrid::getGlobalSize(void) const
   {
      StdSize retvalue = 1;
      std::vector<StdSize> shape_ = this->getGlobalShape();
      for (StdSize s = 0; s < shape_.size(); s++)
         retvalue *= shape_[s];
      return (retvalue);
   }
*/
   StdSize CGrid::getDataSize(void) const
   {
      StdSize retvalue=domain->data_ni.getValue() ;
      if (domain->data_dim.getValue()==2) retvalue*=domain->data_nj.getValue() ;
      if (this->withAxis) retvalue*=this->axis->size.getValue() ;

      return (retvalue);
   }

   std::map<int, StdSize> CGrid::getConnectedServerDataSize()
   {
     double secureFactor = 2.5 * sizeof(double) * CXios::bufferServerFactorSize;
     StdSize retVal;
     std::map<int, StdSize> ret;
     const std::map<int, std::vector<int> >& distribution = clientServerMap_.getLocalIndexSendToServer();
     std::map<int, std::vector<int> >::const_iterator it = distribution.begin(), itE = distribution.end();
     for (; it != itE; ++it)
     {
        retVal = it->second.size();
        retVal *= secureFactor;
        ret.insert(std::make_pair<int,StdSize>(it->first, retVal));
     }

     return ret;
   }


   //---------------------------------------------------------------

//   void CGrid::solveReference(void)
//   {
//      if (this->isChecked) return;
//      CContext* context = CContext::getCurrent() ;
//      CContextClient* client=context->client ;
//
//      this->solveDomainRef() ;
//      this->solveAxisRef() ;
//
//      if (context->hasClient)
//      {
//         checkMask() ;
//         this->computeIndex() ;
//
//         this->storeIndex.push_front(new CArray<int,1>() );
//         this->out_i_index.push_front(new CArray<int,1>());
//         this->out_j_index.push_front(new CArray<int,1>());
//         this->out_l_index.push_front(new CArray<int,1>());
//      }
////      this->computeIndexServer();
//      this->isChecked = true;
//   }

   void CGrid::solveDomainAxisRef(bool areAttributesChecked)
   {
     if (this->isDomainAxisChecked) return;

     this->solveDomainRef(areAttributesChecked);
     this->solveAxisRef(areAttributesChecked);

     this->isDomainAxisChecked = areAttributesChecked;
   }

   void CGrid::checkMaskIndex(bool doSendingIndex)
   {
     CContext* context = CContext::getCurrent() ;
     CContextClient* client=context->client ;

     if (context->hasClient)
      if (this->isChecked && doSendingIndex && !isIndexSent) { sendIndex(); this->isIndexSent = true; }

     if (this->isChecked) return;

     if (context->hasClient)
     {
        checkMask() ;
        this->computeIndex() ;

        this->storeIndex.push_front(new CArray<int,1>() );
        this->out_i_index.push_front(new CArray<int,1>());
        this->out_j_index.push_front(new CArray<int,1>());
        this->out_l_index.push_front(new CArray<int,1>());
     }
//      this->computeIndexServer();
     this->isChecked = true;
   }


   void CGrid::checkMask(void)
   {
      using namespace std;

      unsigned int niu = domain->ni, nju = domain->nj;
      unsigned int nlu = 1 ;
      if (hasAxis()) nlu=axis->size ;

      if (!mask.isEmpty())
      {
         if ((mask.extent(0) != niu) ||
             (mask.extent(1) != nju) ||
             (mask.extent(2) != nlu))
             ERROR("CGrid::checkAttributes(void)",
                  <<"The mask has not the same size than the local grid"<<endl
                  <<"Local size is "<<niu<<"x"<<nju<<"x"<<nlu<<endl
                  <<"Mask size is "<<mask.extent(0)<<"x"<<mask.extent(1)<<"x"<<mask.extent(2));
      }
      else
      {
        mask.resize(niu,nju,nlu) ;
        mask=true  ;
      }

      CArray<bool,2>& domainMask = domain->mask ;
      for (int l=0; l < nlu ; l++)
        for (int j=0; j < nju ; j++)
          for(int i=0; i<niu ; i++) mask(i,j,l) = mask(i,j,l) && domainMask(i,j) ;


   }

   //---------------------------------------------------------------

//   void CGrid::solveDomainRef(void)
//   {
//      if (!domain_ref.isEmpty())
//      {
//         if (CDomain::has(domain_ref.getValue()))
//         {
//            this->domain = CDomain::get(domain_ref.getValue()) ;
//            domain->checkAttributes() ;
//         }
//         else ERROR("CGrid::solveDomainRef(void)",
//                     << "Wrong domain reference") ;
//      }
//      else ERROR("CGrid::solveDomainRef(void)",
//                  << "Domain reference is not defined") ;
//   }
//
//   //---------------------------------------------------------------
//
//   void CGrid::solveAxisRef(void)
//   {
//      if (!axis_ref.isEmpty())
//      {
//         this->withAxis = true ;
//         if (CAxis::get(axis_ref.getValue()))
//         {
//            this->axis = CAxis::get(axis_ref.getValue()) ;
//            axis->checkAttributes() ;
//         }
//         else ERROR("CGrid::solveAxisRef(void)",
//                    << "Wrong axis reference") ;
//      }
//      else withAxis = false ;
//   }


   void CGrid::solveDomainRef(bool sendAtt)
   {
//      if (!domain_ref.isEmpty())
//      {
//         if (CDomain::has(domain_ref.getValue()))
//         {
//            this->domain = CDomain::get(domain_ref.getValue()) ;
//            if (sendAtt) domain->sendCheckedAttributes();
//            else domain->checkAttributesOnClient() ;
//         }
//         else ERROR("CGrid::solveDomainRef(void)",
//                     << "Wrong domain reference") ;
//      }
//      else ERROR("CGrid::solveDomainRef(void)",
//                  << "Domain reference is not defined") ;
      setDomainList();
      this->domain = CDomain::get(domList_.at(0));
      if (0 != this->domain)
      {
//        this->domain = this->getDomain();
        if (sendAtt) domain->sendCheckedAttributes();
        else domain->checkAttributesOnClient() ;
      }
      else ERROR("CGrid::solveDomainRef(void)",
                  << "Domain reference is not defined") ;
   }

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(bool checkAtt)
   {
//      if (!axis_ref.isEmpty())
//      {
//         this->withAxis = true ;
//         if (CAxis::get(axis_ref.getValue()))
//         {
//            this->axis = CAxis::get(axis_ref.getValue()) ;
//            axis->checkAttributes() ;
//         }
//         else ERROR("CGrid::solveAxisRef(void)",
//                    << "Wrong axis reference") ;
//      }
//      else withAxis = false ;
//      getAllAxis();
      setAxisList();
      if (!axisList_.empty())
      {
        int sizeList = axisList_.size();
        for (int i = 0; i < sizeList; ++i)
        {
          CAxis::get(axisList_.at(i))->checkAttributes();
          this->axis = CAxis::get(axisList_.at(i));
        }
        withAxis = true;

      }
//      if (!axis_ref.isEmpty())
//      {
//         this->withAxis = true ;
//         if (CAxis::get(axis_ref.getValue()))
//         {
//            this->axis = CAxis::get(axis_ref.getValue()) ;
//            axis->checkAttributes() ;
//         }
//         else ERROR("CGrid::solveAxisRef(void)",
//                    << "Wrong axis reference") ;
//      }
      else withAxis = false ;
   }

   //---------------------------------------------------------------

   void CGrid::computeIndex(void)
   {
     CContext* context = CContext::getCurrent() ;
     CContextClient* client=context->client ;

     // First of all, compute distribution on client side
     clientDistribution_ = new CDistributionClient(client->clientRank, this);

     // Then compute distribution on server side
     serverDistributionDescription_ = new CServerDistributionDescription(clientDistribution_->getNGlob());
     serverDistributionDescription_->computeServerDistribution(client->serverSize, true);

     // Finally, compute index mapping between client(s) and server(s)
     clientServerMap_.computeServerIndexMapping(clientDistribution_->getGlobalIndex(),serverDistributionDescription_->getGlobalIndex());
     const std::map<int, std::vector<size_t> >& globalIndexOnServer = clientServerMap_.getGlobalIndexOnServer();
     std::vector<int> connectedServerRank;
     for (std::map<int, std::vector<size_t> >::const_iterator it = globalIndexOnServer.begin(); it != globalIndexOnServer.end(); ++it) {
       connectedServerRank.push_back(it->first);
     }
     nbSenders = clientServerMap_.computeConnectedClients(client->serverSize, client->clientSize, client->intraComm, connectedServerRank);

     // Get local data index on client
     storeIndex_client.resize(clientDistribution_->getLocalDataIndexOnClient().numElements());
     storeIndex_client = (clientDistribution_->getLocalDataIndexOnClient());

/*
      const int ni   = domain->ni.getValue() ,
                nj   = domain->nj.getValue() ,
                size = (this->hasAxis()) ? axis->size.getValue() : 1 ,
                lbegin = (this->hasAxis()) ? axis->zoom_begin.getValue()-1 : 0 ,
                lend = (this->hasAxis()) ? axis->zoom_end.getValue()-1 : 0 ;


      const int data_dim     = domain->data_dim.getValue() ,
                data_n_index = domain->data_n_index.getValue() ,
                data_ibegin  = domain->data_ibegin.getValue() ,
                data_jbegin  = (data_dim == 2)
                             ? domain->data_jbegin.getValue() : -1;

      CArray<int,1> data_i_index = domain->data_i_index ;
      CArray<int,1> data_j_index = domain->data_j_index ;


      int indexCount = 0;

      for(int l = 0; l < size ; l++)
      {
         for(int n = 0, i = 0, j = 0; n < data_n_index; n++)
         {
            int temp_i = data_i_index(n) + data_ibegin,
                temp_j = (data_dim == 1) ? -1
                       : data_j_index(n) + data_jbegin;
            i = (data_dim == 1) ? (temp_i - 1) % ni
                                : (temp_i - 1) ;
            j = (data_dim == 1) ? (temp_i - 1) / ni
                                : (temp_j - 1) ;

            if ((l >=lbegin && l<= lend) &&
                (i >= 0 && i < ni) &&
                (j >= 0 && j < nj) && mask(i,j,l))
               indexCount++ ;
         }
      }

      storeIndex[0]  = new CArray<int,1>(indexCount) ;
      out_i_index[0] = new CArray<int,1>(indexCount) ;
      out_j_index[0] = new CArray<int,1>(indexCount) ;
      out_l_index[0] = new CArray<int,1>(indexCount) ;

      storeIndex_client.resize(indexCount) ;
      out_i_client.resize(indexCount) ;
      out_j_client.resize(indexCount) ;
      out_l_client.resize(indexCount) ;


      for(int count = 0, indexCount = 0,  l = 0; l < size; l++)
      {
         for(int n = 0, i = 0, j = 0; n < data_n_index; n++, count++)
         {
            int temp_i = data_i_index(n) + data_ibegin,
                temp_j = (data_dim == 1) ? -1
                       : data_j_index(n) + data_jbegin;
            i = (data_dim == 1) ? (temp_i - 1) % ni
                                : (temp_i - 1) ;
            j = (data_dim == 1) ? (temp_i - 1) / ni
                                : (temp_j - 1) ;

            if ((l >= lbegin && l <= lend) &&
                (i >= 0 && i < ni) &&
                (j >= 0 && j < nj) && mask(i,j,l))
            {
               (*storeIndex[0])(indexCount) = count ;
               (*out_l_index[0])(indexCount) = l ;
               (*out_i_index[0])(indexCount) = i ;
               (*out_j_index[0])(indexCount) = j ;

               storeIndex_client(indexCount) = count ;
               out_i_client(indexCount)=i+domain->ibegin_client-1 ;
               out_j_client(indexCount)=j+domain->jbegin_client-1 ;
               out_l_client(indexCount)=l-lbegin ;
               indexCount++ ;
            }
         }
      }
*/
//      computeDomConServer();
//      sendIndex() ;


   }

   //----------------------------------------------------------------

   CGrid* CGrid::createGrid(CDomain* domain)
   {
      StdString new_id = StdString("__") + domain->getId() + StdString("__") ;
      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;

      std::vector<CDomain*> vecDom(1,domain);
      grid->setDomainList(vecDom);
//      grid->domain_ref.setValue(domain->getId());
      return (grid);
   }

   CGrid* CGrid::createGrid(CDomain* domain, CAxis* axis)
   {
      StdString new_id = StdString("__") + domain->getId() +
                         StdString("_") + axis->getId() + StdString("__") ;
      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;

      std::vector<CDomain*> vecDom(1,domain);
      std::vector<CAxis*> vecAxis(1,axis);
      grid->setDomainList(vecDom);
      grid->setAxisList(vecAxis);
//      grid->domain_ref.setValue(domain->getId());
//      grid->axis_ref.setValue(axis->getId());
      return (grid);
   }

   CGrid* CGrid::createGrid(std::vector<CDomain*> domains, std::vector<CAxis*> axis)
   {
      StdString new_id = StdString("__");
      if (!domains.empty()) for (int i = 0; i < domains.size(); ++i) new_id += domains[i]->getId() + StdString("_");
      if (!axis.empty()) for (int i = 0; i < axis.size(); ++i) new_id += axis[i]->getId() + StdString("_") ;
      new_id += StdString("_");

      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;
      grid->setDomainList(domains);
      grid->setAxisList(axis);

      //By default, domains are always the first ones of a grid
      if (grid->axisDomainOrder.isEmpty())
      {
        int size = domains.size()+axis.size();
        grid->axisDomainOrder.resize(size);
        for (int i = 0; i < size; ++i)
        {
          if (i < domains.size()) grid->axisDomainOrder(i) = true;
          else grid->axisDomainOrder(i) = false;
        }
      }

      return (grid);
   }

   CDomainGroup* CGrid::getVirtualDomainGroup() const
   {
     return (this->vDomainGroup_);
   }

   CAxisGroup* CGrid::getVirtualAxisGroup() const
   {
     return (this->vAxisGroup_);
   }

   //----------------------------------------------------------------

   void CGrid::outputField(int rank, const CArray<double, 1>& stored,  CArray<double, 3>& field)
   {
      CArray<int,1>& out_i=*out_i_fromClient[rank] ;
      CArray<int,1>& out_j=*out_j_fromClient[rank] ;
      CArray<int,1>& out_l=*out_l_fromClient[rank] ;

      for(StdSize n = 0; n < stored.numElements(); n++)
         field(out_i(n), out_j(n), out_l(n)) = stored(n) ;
   }

   void CGrid::outputField(int rank, const CArray<double, 1>& stored,  CArray<double, 2>& field)
   {
      CArray<int,1>& out_i=*out_i_fromClient[rank] ;
      CArray<int,1>& out_j=*out_j_fromClient[rank] ;

      for(StdSize n = 0; n < stored.numElements(); n++)
         field(out_i(n), out_j(n)) = stored(n) ;   }

   //---------------------------------------------------------------

   void CGrid::outputField(int rank,const CArray<double, 1>& stored,  CArray<double, 1>& field)
   {
      CArray<int,1>& out_i=*out_i_fromClient[rank] ;

      for(StdSize n = 0; n < stored.numElements(); n++)
         field(out_i(n)) = stored(n) ;
   }

   void CGrid::outputField(int rank, const CArray<double, 1>& stored, double* field)
   {
     CArray<size_t,1>& out_i=*outIndexFromClient[rank];
     StdSize numElements = stored.numElements();
     for (StdSize n = 0; n < numElements; ++n)
     {
       *(field+out_i(n)) = stored(n);
     }
   }

   //----------------------------------------------------------------


   void CGrid::storeField_arr
      (const double * const data, CArray<double, 1>& stored) const
   {
      const StdSize size = storeIndex_client.numElements() ;

      stored.resize(size) ;
      for(StdSize i = 0; i < size; i++) stored(i) = data[storeIndex_client(i)] ;
   }

   //---------------------------------------------------------------

//  void CGrid::sendIndex(void)
//  {
//    CContext* context = CContext::getCurrent() ;
//    CContextClient* client=context->client ;
//
//    CEventClient event(getType(),EVENT_ID_INDEX) ;
//    int rank ;
//    list<shared_ptr<CMessage> > list_msg ;
//    list< CArray<int,1>* > list_out_i,list_out_j,list_out_l ;
//
//    for(int ns=0;ns<domain->connectedServer.size();ns++)
//    {
//       rank=domain->connectedServer[ns] ;
//
//       int i,j ;
//       int nb=0 ;
//       for(int k=0;k<storeIndex_client.numElements();k++)
//       {
//         i=out_i_client(k)- domain->ibegin +1;
//         j=out_j_client(k)- domain->jbegin +1;
//         if (domain->mapConnectedServer(i,j)==ns)  nb++ ;
//       }
//       CArray<int,1> storeIndex(nb) ;
//       CArray<int,1> out_i(nb) ;
//       CArray<int,1> out_j(nb) ;
//       CArray<int,1> out_l(nb) ;
//
//
//       nb=0 ;
//       for(int k=0;k<storeIndex_client.numElements();k++)
//       {
//         i=out_i_client(k)- domain->ibegin +1 ;
//         j=out_j_client(k)- domain->jbegin +1 ;
//         if (domain->mapConnectedServer(i,j)==ns)
//         {
//            storeIndex(nb)=k ;
//            out_i(nb)=domain->i_index(i,j) + domain->ibegin-1;
//            out_j(nb)=domain->j_index(i,j) + domain->jbegin-1;
//            out_l(nb)=out_l_client(k) ;
//            nb++ ;
//         }
//       }
//
//       storeIndex_toSrv.insert( pair<int,CArray<int,1>* >(rank,new CArray<int,1>(storeIndex) )) ;
//       nbSenders.insert(pair<int,int>(rank,domain->nbSenders[ns])) ;
//       list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
//       list_out_i.push_back(new CArray<int,1>(out_i)) ;
//       list_out_j.push_back(new CArray<int,1>(out_j)) ;
//       list_out_l.push_back(new CArray<int,1>(out_l)) ;
//
//       *list_msg.back()<<getId()<<*list_out_i.back()<<*list_out_j.back()<<*list_out_l.back() ;
//       event.push(rank,domain->nbSenders[ns],*list_msg.back()) ;
//    }
//    client->sendEvent(event) ;
//
//    for(list<CArray<int,1>* >::iterator it=list_out_i.begin();it!=list_out_i.end();it++) delete *it ;
//    for(list<CArray<int,1>* >::iterator it=list_out_j.begin();it!=list_out_j.end();it++) delete *it ;
//    for(list<CArray<int,1>* >::iterator it=list_out_l.begin();it!=list_out_l.end();it++) delete *it ;
//
//  }

  void CGrid::computeDomConServer()
  {
    if (!isDomConServerComputed_)
    {
      for(int ns=0;ns<domain->connectedServer.size(); ++ns)
      {
         int rank=domain->connectedServer[ns] ;

         int i,j ;
         int nb=0 ;
         for(int k=0;k<storeIndex_client.numElements();++k)
         {
           i=out_i_client(k)- domain->ibegin +1;
           j=out_j_client(k)- domain->jbegin +1;
           if (domain->mapConnectedServer(i,j)==ns)  ++nb ;
         }

         domConnectedServerSide_.insert(std::make_pair(rank, nb));
      }
      isDomConServerComputed_ = true;
    }
  }


  std::map<int, int> CGrid::getDomConServerSide()
  {
    return domConnectedServerSide_;
  }

  void CGrid::sendIndex(void)
  {
    CContext* context = CContext::getCurrent() ;
    CContextClient* client=context->client ;

    CEventClient event(getType(),EVENT_ID_INDEX) ;
    int rank ;
    list<shared_ptr<CMessage> > list_msg ;
    list< CArray<size_t,1>* > listOutIndex;
    const std::map<int, std::vector<size_t> >& globalIndexOnServer = clientServerMap_.getGlobalIndexOnServer();
    const std::map<int, std::vector<int> >& localIndexSendToServer  = clientServerMap_.getLocalIndexSendToServer();

    std::map<int, std::vector<size_t> >::const_iterator iteMap, itbMap, itGlobal;
    std::map<int, std::vector<int> >::const_iterator itLocal;
    itbMap = itGlobal = globalIndexOnServer.begin();
    iteMap = globalIndexOnServer.end();
    itLocal = localIndexSendToServer.begin();

    for (int ns = 0; itGlobal != iteMap; ++itGlobal, ++itLocal, ++ns)
    {
      rank = itGlobal->first;
      int nb = (itGlobal->second).size();

      CArray<size_t, 1> outGlobalIndexOnServer(nb);
      CArray<int, 1> outLocalIndexToServer(nb);
      for (int k = 0; k < nb; ++k)
      {
        outGlobalIndexOnServer(k) = itGlobal->second.at(k);
        outLocalIndexToServer(k)  = itLocal->second.at(k);
      }

      storeIndex_toSrv.insert( pair<int,CArray<int,1>* >(rank,new CArray<int,1>(outLocalIndexToServer) ));
      listOutIndex.push_back(new CArray<size_t,1>(outGlobalIndexOnServer));

      list_msg.push_back(shared_ptr<CMessage>(new CMessage));
      *list_msg.back()<<getId()<<*listOutIndex.back();
      event.push(rank, nbSenders[rank], *list_msg.back());
    }
    client->sendEvent(event);
    for(list<CArray<size_t,1>* >::iterator it=listOutIndex.begin();it!=listOutIndex.end();++it) delete *it ;

/*
    if (!isDomConServerComputed_) computeDomConServer();

    for(int ns=0;ns<domain->connectedServer.size();ns++)
    {
       rank=domain->connectedServer[ns] ;

       int nb = domConnectedServerSide_.find(rank)->second;
       CArray<int,1> storeIndex(nb) ;
       CArray<int,1> out_i(nb) ;
       CArray<int,1> out_j(nb) ;
       CArray<int,1> out_l(nb) ;

       int i, j;
       nb=0 ;
       for(int k=0;k<storeIndex_client.numElements();k++)
       {
         i=out_i_client(k)- domain->ibegin +1 ;
         j=out_j_client(k)- domain->jbegin +1 ;
         if (domain->mapConnectedServer(i,j)==ns)
         {
            storeIndex(nb)=k ;
            out_i(nb)=domain->i_index(i,j) + domain->ibegin-1;
            out_j(nb)=domain->j_index(i,j) + domain->jbegin-1;
            out_l(nb)=out_l_client(k) ;
            nb++ ;
         }
       }

       storeIndex_toSrv.insert( pair<int,CArray<int,1>* >(rank,new CArray<int,1>(storeIndex) )) ;
       nbSenders.insert(pair<int,int>(rank,domain->nbSenders[ns])) ;
       list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
       list_out_i.push_back(new CArray<int,1>(out_i)) ;
       list_out_j.push_back(new CArray<int,1>(out_j)) ;
       list_out_l.push_back(new CArray<int,1>(out_l)) ;

       *list_msg.back()<<getId()<<*list_out_i.back()<<*list_out_j.back()<<*list_out_l.back() ;
       event.push(rank,domain->nbSenders[ns],*list_msg.back()) ;
    }
    client->sendEvent(event) ;

    for(list<CArray<int,1>* >::iterator it=list_out_i.begin();it!=list_out_i.end();it++) delete *it ;
    for(list<CArray<int,1>* >::iterator it=list_out_j.begin();it!=list_out_j.end();it++) delete *it ;
    for(list<CArray<int,1>* >::iterator it=list_out_l.begin();it!=list_out_l.end();it++) delete *it ;
*/
  }

  void CGrid::recvIndex(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it ;
    for (it=event.subEvents.begin();it!=event.subEvents.end();++it)
    {
      int rank=it->rank;
      CBufferIn* buffer=it->buffer;
      string gridId ;
      *buffer>>gridId ;
      get(gridId)->recvIndex(rank,*buffer) ;
    }
  }

  void CGrid::recvIndex(int rank, CBufferIn& buffer)
  {
     if (0 == serverDistribution_)
     {
       CContext* context = CContext::getCurrent() ;
       CContextServer* server=context->server ;
       int idx = 0, numElement = axisDomainOrder.numElements();
       int ssize = numElement;
       std::vector<int> indexMap(numElement);
       for (int i = 0; i < numElement; ++i)
       {
         indexMap[i] = idx;
         if (true == axisDomainOrder(i))
        {
          ++ssize;
          idx += 2;
        }
       }

       int axisId = 0, domainId = 0;
       std::vector<CDomain*> domainList = getDomains();
       std::vector<CAxis*> axisList = getAxis();
       std::vector<int> nZoomBegin(ssize), nZoomSize(ssize), nGlob(ssize);
       for (int i = 0; i < numElement; ++i)
       {
         if (axisDomainOrder(i))
         {
            nZoomBegin[indexMap[i]]   = domainList[domainId]->zoom_ibegin_srv;
            nZoomSize[indexMap[i]]    = domainList[domainId]->zoom_ni_srv;
            nGlob[indexMap[i]]    = domainList[domainId]->ni_glo;

            nZoomBegin[indexMap[i]+1] = domainList[domainId]->zoom_jbegin_srv;
            nZoomSize[indexMap[i]+1]  = domainList[domainId]->zoom_nj_srv;
            nGlob[indexMap[i]+1]    = domainList[domainId]->nj_glo;
            ++domainId;
         }
         else
         {
            nZoomBegin[indexMap[i]] = axisList[axisId]->zoom_begin;
            nZoomSize[indexMap[i]]  = axisList[axisId]->zoom_size;
            nGlob[indexMap[i]]      = axisList[axisId]->size;
            ++axisId;
         }
       }
       serverDistribution_ = new CDistributionServer(server->intraCommRank, nZoomBegin, nZoomSize, nGlob);
     }

     CArray<size_t,1> outIndex;
     buffer>>outIndex;
     serverDistribution_->computeLocalIndex(outIndex);
     outIndexFromClient.insert(std::pair<int, CArray<size_t,1>* >(rank, new CArray<size_t,1>(outIndex)));

    /*
    CArray<int,1> out_i ;
    CArray<int,1> out_j ;
    CArray<int,1> out_l ;

    buffer>>out_i>>out_j>>out_l ;

    out_i -= domain->zoom_ibegin_srv-1 ;
    out_j -= domain->zoom_jbegin_srv-1 ;

    out_i_fromClient.insert(pair< int,CArray<int,1>* >(rank,new CArray<int,1>(out_i) )) ;
    out_j_fromClient.insert(pair< int,CArray<int,1>* >(rank,new CArray<int,1>(out_j) )) ;
    out_l_fromClient.insert(pair< int,CArray<int,1>* >(rank,new CArray<int,1>(out_l) )) ;
    */
  }

   /*!
   \brief Dispatch event received from client
      Whenever a message is received in buffer of server, it will be processed depending on
   its event type. A new event type should be added in the switch list to make sure
   it processed on server side.
   \param [in] event: Received message
   */
  bool CGrid::dispatchEvent(CEventServer& event)
  {

    if (SuperClass::dispatchEvent(event)) return true ;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_INDEX :
          recvIndex(event) ;
          return true ;
          break ;

         case EVENT_ID_ADD_DOMAIN :
           recvAddDomain(event) ;
           return true ;
           break ;

         case EVENT_ID_ADD_AXIS :
           recvAddAxis(event) ;
           return true ;
           break ;
        default :
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                <<"Unknown Event") ;
          return false ;
      }
    }
  }

   void CGrid::inputFieldServer(const std::deque< CArray<double, 1>* > storedClient, CArray<double, 1>&  storedServer) const
   {
      if ((this->storeIndex.size()-1 ) != storedClient.size())
         ERROR("void CGrid::inputFieldServer(const std::deque< CArray<double, 1>* > storedClient, CArray<double, 1>&  storedServer) const",
                << "[ Expected received field = " << (this->storeIndex.size()-1) << ", "
                << "[ received fiedl = "    << storedClient.size() << "] "
                << "Data from clients are missing!") ;
      storedServer.resize(storeIndex[0]->numElements());

      for (StdSize i = 0, n = 0; i < storedClient.size(); i++)
         for (StdSize j = 0; j < storedClient[i]->numElements(); j++)
            storedServer(n++) = (*storedClient[i])(j);
   }

   void CGrid::outputFieldToServer(CArray<double,1>& fieldIn, int rank, CArray<double,1>& fieldOut)
   {
     CArray<int,1>& index = *storeIndex_toSrv[rank] ;
     int nb=index.numElements() ;
     fieldOut.resize(nb) ;

     for(int k=0;k<nb;k++) fieldOut(k)=fieldIn(index(k)) ;
    }
   ///---------------------------------------------------------------

   CDomain* CGrid::addDomain(const std::string& id)
   {
     return vDomainGroup_->createChild(id) ;
   }

   CAxis* CGrid::addAxis(const std::string& id)
   {
     return vAxisGroup_->createChild(id) ;
   }

   //! Change virtual field group to a new one
   void CGrid::setVirtualDomainGroup(CDomainGroup* newVDomainGroup)
   {
      this->vDomainGroup_ = newVDomainGroup;
   }

   //! Change virtual variable group to new one
   void CGrid::setVirtualAxisGroup(CAxisGroup* newVAxisGroup)
   {
      this->vAxisGroup_ = newVAxisGroup;
   }

   //----------------------------------------------------------------
   //! Create virtual field group, which is done normally on initializing file
   void CGrid::setVirtualDomainGroup(void)
   {
      this->setVirtualDomainGroup(CDomainGroup::create());
   }

   //! Create virtual variable group, which is done normally on initializing file
   void CGrid::setVirtualAxisGroup(void)
   {
      this->setVirtualAxisGroup(CAxisGroup::create());
   }

   /*!
   \brief Send a message to create a domain on server side
   \param[in] id String identity of domain that will be created on server
   */
   void CGrid::sendAddDomain(const string& id)
   {
    CContext* context=CContext::getCurrent() ;

    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_ADD_DOMAIN) ;
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<id ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
   }

   /*!
   \brief Send a message to create an axis on server side
   \param[in] id String identity of axis that will be created on server
   */
   void CGrid::sendAddAxis(const string& id)
   {
    CContext* context=CContext::getCurrent() ;

    if (! context->hasServer )
    {
       CContextClient* client=context->client ;

       CEventClient event(this->getType(),EVENT_ID_ADD_AXIS) ;
       if (client->isServerLeader())
       {
         CMessage msg ;
         msg<<this->getId() ;
         msg<<id ;
         event.push(client->getServerLeader(),1,msg) ;
         client->sendEvent(event) ;
       }
       else client->sendEvent(event) ;
    }
   }

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] event Received event
   */
   void CGrid::recvAddDomain(CEventServer& event)
   {

      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvAddDomain(*buffer) ;
   }

   /*!
   \brief Receive a message annoucing the creation of a domain on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddDomain(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      addDomain(id) ;
   }

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] event Received event
   */
   void CGrid::recvAddAxis(CEventServer& event)
   {

      CBufferIn* buffer=event.subEvents.begin()->buffer;
      string id;
      *buffer>>id ;
      get(id)->recvAddAxis(*buffer) ;
   }

   /*!
   \brief Receive a message annoucing the creation of an axis on server side
   \param[in] buffer Buffer containing message
   */
   void CGrid::recvAddAxis(CBufferIn& buffer)
   {
      string id ;
      buffer>>id ;
      addAxis(id) ;
   }

  /*!
  \brief Solve domain and axis references
  As field, domain and axis can refer to other domains or axis. In order to inherit correctly
  all attributes from their parents, they should be processed with this function
  \param[in] apply inherit all attributes of parents (true)
  */
  void CGrid::solveDomainAxisRefInheritance(bool apply)
  {
    CContext* context = CContext::getCurrent();
    unsigned int vecSize, i;
    std::vector<StdString>::iterator it, itE;
    setDomainList();
    it = domList_.begin(); itE = domList_.end();
    for (; it != itE; ++it)
    {
      CDomain* pDom = CDomain::get(*it);
      if (context->hasClient)
      {
        pDom->solveRefInheritance(apply);
        pDom->solveBaseReference();
        if ((!pDom->domain_ref.isEmpty()) && (pDom->name.isEmpty()))
          pDom->name.setValue(pDom->getBaseDomainReference()->getId());
      }
    }

    setAxisList();
    it = axisList_.begin(); itE = axisList_.end();
    for (; it != itE; ++it)
    {
      CAxis* pAxis = CAxis::get(*it);
      if (context->hasClient)
      {
        pAxis->solveRefInheritance(apply);
        pAxis->solveBaseReference();
        if ((!pAxis->axis_ref.isEmpty()) && (pAxis->name.isEmpty()))
          pAxis->name.setValue(pAxis->getBaseAxisReference()->getId());
      }
    }
  }

  /*!
  \brief Get the list of domain pointers
  \return list of domain pointers
  */
  std::vector<CDomain*> CGrid::getDomains()
  {
    std::vector<CDomain*> domList;
    if (!domList_.empty())
    {
      for (int i = 0; i < domList_.size(); ++i) domList.push_back(CDomain::get(domList_[i]));
    }
    return domList;
  }

  /*!
  \brief Get the list of  axis pointers
  \return list of axis pointers
  */
  std::vector<CAxis*> CGrid::getAxis()
  {
    std::vector<CAxis*> aList;
    if (!axisList_.empty())
      for (int i =0; i < axisList_.size(); ++i) aList.push_back(CAxis::get(axisList_[i]));

    return aList;
  }

  /*!
  \brief Set domain(s) of a grid from a list
  \param[in] domains list of domains
  */
  void CGrid::setDomainList(const std::vector<CDomain*> domains)
  {
    if (isDomListSet) return;
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    if (!domains.empty() && domList.empty()) domList = domains;
    if (!domList.empty())
    {
      int sizeDom = domList.size();
      domList_.resize(sizeDom);
      for (int i = 0 ; i < sizeDom; ++i)
      {
        domList_[i] = domList[i]->getId();
      }
      isDomListSet = true;
    }

  }

  /*!
  \brief Set axis(s) of a grid from a list
  \param[in] axis list of axis
  */
  void CGrid::setAxisList(const std::vector<CAxis*> axis)
  {
    if (isAxisListSet) return;
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    if (!axis.empty() && aList.empty()) aList = axis;
    if (!aList.empty())
    {
      int sizeAxis = aList.size();
      axisList_.resize(sizeAxis);
      for (int i = 0; i < sizeAxis; ++i)
      {
        axisList_[i] = aList[i]->getId();
      }
      isAxisListSet = true;
    }
  }

  /*!
  \brief Get list of id of domains
  \return id list of domains
  */
  std::vector<StdString> CGrid::getDomainList()
  {
    setDomainList();
    return domList_;
  }

  /*!
  \brief Get list of id of axis
  \return id list of axis
  */
  std::vector<StdString> CGrid::getAxisList()
  {
    setAxisList();
    return axisList_;
  }

  void CGrid::sendAllDomains()
  {
    std::vector<CDomain*> domList = this->getVirtualDomainGroup()->getAllChildren();
    int dSize = domList.size();
    for (int i = 0; i < dSize; ++i)
    {
      sendAddDomain(domList[i]->getId());
      domList[i]->sendAllAttributesToServer();
    }
  }

  void CGrid::sendAllAxis()
  {
    std::vector<CAxis*> aList = this->getVirtualAxisGroup()->getAllChildren();
    int aSize = aList.size();

    for (int i = 0; i < aSize; ++i)
    {
      sendAddAxis(aList[i]->getId());
      aList[i]->sendAllAttributesToServer();
    }
  }

  void CGrid::parse(xml::CXMLNode & node)
  {
    SuperClass::parse(node);
    // List order of axis and domain in a grid, if there is a domain, it will take value 1 (true), axis 0 (false)
//    std::vector<int> axisOrder;
    std::vector<bool> order;

    if (node.goToChildElement())
    {
      StdString domainName("domain");
      StdString axisName("axis");
      do
      {
        if (node.getElementName() == domainName) {
          order.push_back(true);
          this->getVirtualDomainGroup()->parseChild(node);
        }
        if (node.getElementName() == axisName) {
          order.push_back(false);
          this->getVirtualAxisGroup()->parseChild(node);
        }
      } while (node.goToNextElement()) ;
      node.goToParentElement();
    }

    if (!order.empty())
    {
      int sizeOrd = order.size();
      axisDomainOrder.resize(sizeOrd);
      for (int i = 0; i < sizeOrd; ++i)
      {
        axisDomainOrder(i) = order[i];
      }
    }

    setDomainList();
    setAxisList();
   }

} // namespace xios
