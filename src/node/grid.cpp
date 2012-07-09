
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

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CGrid::CGrid(void)
      : CObjectTemplate<CGrid>(), CGridAttributes()
      , withAxis(false), isChecked(false), axis(), domain()
      , storeIndex(1), out_i_index(1), out_j_index(1), out_l_index(1)
   { /* Ne rien faire de plus */ }

   CGrid::CGrid(const StdString & id)
      : CObjectTemplate<CGrid>(id), CGridAttributes()
      , withAxis(false), isChecked(false), axis(), domain()
      , storeIndex(1), out_i_index(1), out_j_index(1), out_l_index(1)
   { /* Ne rien faire de plus */ }

   CGrid::~CGrid(void)
   { 
 //     this->axis.reset() ;
//      this->domain.reset() ;
      
      for (StdSize i = 0; i < this->storeIndex.size(); i++)
      {
         this->storeIndex[i].reset();
         this->out_i_index[i].reset();
         this->out_j_index[i].reset();
         this->out_l_index[i].reset();
      }
   }

   ///---------------------------------------------------------------

   StdString CGrid::GetName(void)    { return (StdString("grid")); }
   StdString CGrid::GetDefName(void) { return (CGrid::GetName()); }
   ENodeType CGrid::GetType(void)    { return (eGrid); }

   //----------------------------------------------------------------

   const std::deque<ARRAY(int, 1)> & CGrid::getStoreIndex(void) const
   { 
      return (this->storeIndex );
   }

   //---------------------------------------------------------------

   const std::deque<ARRAY(int, 1)> & CGrid::getOutIIndex(void)  const
   { 
      return (this->out_i_index ); 
   }

   //---------------------------------------------------------------

   const std::deque<ARRAY(int, 1)> & CGrid::getOutJIndex(void)  const
   { 
      return (this->out_j_index ); 
   }

   //---------------------------------------------------------------

   const std::deque<ARRAY(int, 1)> & CGrid::getOutLIndex(void)  const
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
      StdSize retvalue ;
      retvalue=domain->data_ni.getValue() ;
      if (domain->data_dim.getValue()==2) retvalue*=domain->data_nj.getValue() ;
      if (this->withAxis) retvalue*=this->axis->size.getValue() ;

      return (retvalue);
   }

   //---------------------------------------------------------------

   void CGrid::solveReference(void)
   {
      if (this->isChecked) return;
      CContext* context = CContext::getCurrent() ;
      CContextClient* client=context->client ;
      
      this->solveDomainRef() ;
      this->solveAxisRef() ;
      if (context->hasClient)
      {
         
         this->computeIndex() ;
         ARRAY_CREATE(storeIndex_ , int, 1, [0]);
         ARRAY_CREATE(out_l_index_, int, 1, [0]);
         ARRAY_CREATE(out_i_index_, int, 1, [0]);
         ARRAY_CREATE(out_j_index_, int, 1, [0]);
                 
         this->storeIndex .push_front(storeIndex_);
         this->out_i_index.push_front(out_i_index_);
         this->out_j_index.push_front(out_j_index_);
         this->out_l_index.push_front(out_l_index_);
      }
//      this->computeIndexServer();
      this->isChecked = true;
   }

   //---------------------------------------------------------------

   void CGrid::solveDomainRef(void)
   {
      if (!domain_ref.isEmpty())
      {
         if (CDomain::has(domain_ref.getValue()))
         {
            this->domain = CDomain::get(domain_ref.getValue()) ;
            domain->checkAttributes() ;
         }
         else ERROR("CGrid::solveDomainRef(void)",
                     << "Référence au domaine incorrecte") ;
      }
      else ERROR("CGrid::solveDomainRef(void)",
                  << "Domaine non défini") ;
   }

   //---------------------------------------------------------------

   void CGrid::solveAxisRef(void)
   {
      if (!axis_ref.isEmpty())
      {
         this->withAxis = true ;
         if (CAxis::get(axis_ref.getValue()))
         {
            this->axis = CAxis::get(axis_ref.getValue()) ;
            axis->checkAttributes() ;
         }
         else ERROR("CGrid::solveAxisRef(void)",
                    << "Référence a l'axe incorrecte") ;
      }
      else withAxis = false ;
   }

   //---------------------------------------------------------------

   void CGrid::computeIndex(void)
   {    
   
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

      ARRAY(int, 1) data_i_index = domain->data_i_index.getValue() ,
                    data_j_index = domain->data_j_index.getValue() ;


      ARRAY(bool, 2) mask = domain->mask.getValue() ;
      ARRAY(int, 2) local_mask =  this->domain->getLocalMask();

      int indexCount = 0;

      for(int l = 0; l < size ; l++)
      {
         for(int n = 0, i = 0, j = 0; n < data_n_index; n++)
         {
            int temp_i = (*data_i_index)[n] + data_ibegin,
                temp_j = (data_dim == 1) ? -1
                       : (*data_j_index)[n] + data_jbegin;
            i = (data_dim == 1) ? (temp_i - 1) % ni
                                : (temp_i - 1) ;
            j = (data_dim == 1) ? (temp_i - 1) / ni
                                : (temp_j - 1) ;

            if ((l >=lbegin && l<= lend) &&
                (i >= 0 && i < ni) &&
                (j >= 0 && j < nj) && (*mask)[i][j])
               indexCount++ ;
         }
      }
      
      ARRAY_ASSIGN(this->storeIndex[0] , int, 1, [indexCount]);
      ARRAY_ASSIGN(this->out_l_index[0], int, 1, [indexCount]);
      ARRAY_ASSIGN(this->out_i_index[0], int, 1, [indexCount]);
      ARRAY_ASSIGN(this->out_j_index[0], int, 1, [indexCount]);
      
      ARRAY_ASSIGN(storeIndex_client,int,1,[indexCount]);
      ARRAY_ASSIGN(out_i_client,int,1,[indexCount]);
      ARRAY_ASSIGN(out_j_client,int,1,[indexCount]);
      ARRAY_ASSIGN(out_l_client,int,1,[indexCount]);
      
      
      for(int count = 0, indexCount = 0,  l = 0; l < size; l++)
      {
         for(int n = 0, i = 0, j = 0; n < data_n_index; n++, count++)
         {
            int temp_i = (*data_i_index)[n] + data_ibegin,
                temp_j = (data_dim == 1) ? -1
                       : (*data_j_index)[n] + data_jbegin;
            i = (data_dim == 1) ? (temp_i - 1) % ni
                                : (temp_i - 1) ;
            j = (data_dim == 1) ? (temp_i - 1) / ni
                                : (temp_j - 1) ;

            if ((l >= lbegin && l <= lend) &&
                (i >= 0 && i < ni) &&
                (j >= 0 && j < nj) && (*mask)[i][j])
            {
               (*this->storeIndex[0]) [indexCount] = count ;
               (*this->out_l_index[0])[indexCount] = l ;
               (*this->out_i_index[0])[indexCount] = i ;
               (*this->out_j_index[0])[indexCount] = j ;
               
               (*storeIndex_client)[indexCount] = count ;
               (*out_i_client)[indexCount]=i+domain->ibegin_client-1 ;
               (*out_j_client)[indexCount]=j+domain->jbegin_client-1 ;
               (*out_l_client)[indexCount]=l-lbegin ;
               indexCount++ ;
            }
         }
      }
      sendIndex() ;


   }

   //----------------------------------------------------------------

   CGrid* CGrid::createGrid(CDomain* domain)
   {
      StdString new_id = StdString("__") + domain->getId() + StdString("__") ;
      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;
      grid->domain_ref.setValue(domain->getId());
      return (grid);
   }

   CGrid* CGrid::createGrid(CDomain* domain, CAxis* axis)
   {
      StdString new_id = StdString("__") + domain->getId() +
                         StdString("_") + axis->getId() + StdString("__") ;
      CGrid* grid = CGridGroup::get("grid_definition")->createChild(new_id) ;
      grid->domain_ref.setValue(domain->getId());
      grid->axis_ref.setValue(axis->getId());
      return (grid);
   }

   //----------------------------------------------------------------

   void CGrid::outputField(int rank, const ARRAY(double, 1) stored,  ARRAY(double, 3) field) 
   {
      ARRAY(int,1) out_i=out_i_fromClient[rank] ;
      ARRAY(int,1) out_j=out_j_fromClient[rank] ;
      ARRAY(int,1) out_l=out_l_fromClient[rank] ;
      
      for(StdSize n = 0; n < stored->num_elements(); n++)
         (*field)[(*out_i)[n]][(*out_j)[n]][(*out_l)[n]] = (*stored)[n] ;
   }

   void CGrid::outputField(int rank, const ARRAY(double, 1) stored,  ARRAY(double, 2) field) 
   {
      ARRAY(int,1) out_i=out_i_fromClient[rank] ;
      ARRAY(int,1) out_j=out_j_fromClient[rank] ;

       for(StdSize n = 0; n < stored->num_elements(); n++)
         (*field)[(*out_i)[n]][(*out_j)[n]] = (*stored)[n] ;
   }

   //---------------------------------------------------------------

   void CGrid::outputField(int rank,const ARRAY(double, 1) stored,  ARRAY(double, 1) field)
   {
      ARRAY(int,1) out_i = out_i_fromClient[rank] ;
      for(StdSize n = 0; n < stored->num_elements(); n++)
         (*field)[(*out_i)[n]] = (*stored)[n] ;
   }

   //----------------------------------------------------------------
  

   void CGrid::storeField_arr
      (const double * const data, ARRAY(double, 1) stored) const
   {
      const StdSize size = (this->storeIndex_client)->num_elements() ;

      stored->resize(boost::extents[size]) ;
      for(StdSize i = 0; i < size; i++)
         (*stored)[i] = data[(*storeIndex_client)[i]] ;
   }
   
   //---------------------------------------------------------------
   
   void CGrid::toBinary  (StdOStream & os) const
   {
      SuperClass::toBinary(os);
     
      os.write (reinterpret_cast<const char*>(&this->isChecked), sizeof(bool)); 
     
      if (this->isChecked)
      {
         this->storeIndex [0]->toBinary(os);
         this->out_i_index[0]->toBinary(os);
         this->out_j_index[0]->toBinary(os);
         this->out_l_index[0]->toBinary(os);
      }
   }

   //---------------------------------------------------------------
   
   void CGrid::fromBinary(StdIStream & is)
   {
      bool hasIndex = false;
      SuperClass::fromBinary(is);            
      is.read (reinterpret_cast<char*>(&hasIndex), sizeof(bool)); 
      
      if (hasIndex)
      {
         ARRAY_CREATE(storeIndex_ , int, 1, [0]);
         ARRAY_CREATE(out_l_index_, int, 1, [0]);
         ARRAY_CREATE(out_i_index_, int, 1, [0]);
         ARRAY_CREATE(out_j_index_, int, 1, [0]);
         
         storeIndex_ ->fromBinary(is);
         out_i_index_->fromBinary(is);
         out_j_index_->fromBinary(is);
         out_l_index_->fromBinary(is);
         
         this->storeIndex .push_back(storeIndex_);
         this->out_i_index.push_back(out_i_index_);
         this->out_j_index.push_back(out_j_index_);
         this->out_l_index.push_back(out_l_index_);
      }
   }
   
   //---------------------------------------------------------------
  void CGrid::sendIndex(void)
  {
    CContext* context = CContext::getCurrent() ;
    CContextClient* client=context->client ;
    
    CEventClient event(getType(),EVENT_ID_INDEX) ;
    int rank ;
    list<shared_ptr<CMessage> > list_msg ;
    list<ARRAY(int,1) > list_out_i,list_out_j,list_out_l ;
     
    for(int ns=0;ns<domain->connectedServer.size();ns++)
    {
       rank=domain->connectedServer[ns] ;
       int ib=domain->ib_srv[ns] ;
       int ie=domain->ie_srv[ns] ;
       int jb=domain->jb_srv[ns] ;
       int je=domain->je_srv[ns] ;
       
       int i,j ;
       int nb=0 ;
       for(int k=0;k<storeIndex_client->num_elements();k++)
       {
         i=(*out_i_client)[k] ;
         j=(*out_j_client)[k] ;
         if (i>=ib-1 && i<=ie-1 && j>=jb-1 && j<=je-1) nb++ ; 
       }
       
       ARRAY_CREATE(storeIndex,int,1,[nb]) ;
       ARRAY_CREATE(out_i,int,1,[nb]) ;
       ARRAY_CREATE(out_j,int,1,[nb]) ;
       ARRAY_CREATE(out_l,int,1,[nb]) ;
       
       nb=0 ;
       for(int k=0;k<storeIndex_client->num_elements();k++)
       {
         i=(*out_i_client)[k] ;
         j=(*out_j_client)[k] ;
         if (i>=ib-1 && i<=ie-1 && j>=jb-1 && j<=je-1) 
         {
            (*storeIndex)[nb]=k ;
            (*out_i)[nb]=(*out_i_client)[k] ;
            (*out_j)[nb]=(*out_j_client)[k] ;
            (*out_l)[nb]=(*out_l_client)[k] ;
            nb++ ;
         }
       }
       
       storeIndex_toSrv.insert(pair<int,ARRAY(int,1) >(rank,storeIndex)) ;
       nbSenders.insert(pair<int,int>(rank,domain->nbSenders[ns])) ;
       list_msg.push_back(shared_ptr<CMessage>(new CMessage)) ;
       list_out_i.push_back(out_i) ;
       list_out_j.push_back(out_j) ;
       list_out_l.push_back(out_l) ;

       *list_msg.back()<<getId()<<list_out_i.back()<<list_out_j.back()<<list_out_l.back() ;
       event.push(rank,domain->nbSenders[ns],*list_msg.back()) ;
    }
    client->sendEvent(event) ;
  }
  
  void CGrid::recvIndex(CEventServer& event)
  {
    list<CEventServer::SSubEvent>::iterator it ;
    for (it=event.subEvents.begin();it!=event.subEvents.end();++it)
    {
      int rank=it->rank;
      CBufferIn* buffer=it->buffer;
      string domainId ;
      *buffer>>domainId ;
      get(domainId)->recvIndex(rank,*buffer) ;
    }
  }
  
  void CGrid::recvIndex(int rank, CBufferIn& buffer)
  {
    ARRAY_CREATE(out_i,int,1,[0]) ;
    ARRAY_CREATE(out_j,int,1,[0]) ;
    ARRAY_CREATE(out_l,int,1,[0]) ;
    
    buffer>>out_i>>out_j>>out_l ;
    int offset ;
    
    offset=domain->zoom_ibegin_srv-1 ;
    for(int k=0;k<out_i->num_elements();k++) (*out_i)[k]=(*out_i)[k]-offset ;
    
    offset=domain->zoom_jbegin_srv-1 ;
    for(int k=0;k<out_i->num_elements();k++) (*out_j)[k]=(*out_j)[k]-offset ;
    
    out_i_fromClient.insert(pair< int,ARRAY(int,1) >(rank,out_i)) ;
    out_j_fromClient.insert(pair< int,ARRAY(int,1) >(rank,out_j)) ;
    out_l_fromClient.insert(pair< int,ARRAY(int,1) >(rank,out_l)) ;
  }

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
 
        default :
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                <<"Unknown Event") ;
          return false ;
      }
    }
  }


/*
   void CGrid::computeIndexServer(void)
   {
      ARRAY(int, 2) local_mask =  this->domain->getLocalMask();
      
      const std::vector<int> & ibegin = this->domain->getIBeginSub();
      const std::vector<int> & jbegin = this->domain->getJBeginSub();
      const std::vector<int> & iend = this->domain->getIEndSub();
      const std::vector<int> & jend = this->domain->getJEndSub();
      const std::vector<int> & ibegin_zoom = this->domain->getIBeginZoomSub();
      const std::vector<int> & jbegin_zoom = this->domain->getJBeginZoomSub();
      const std::vector<int> & ni_zoom = this->domain->getNiZoomSub();
      const std::vector<int> & nj_zoom = this->domain->getNjZoomSub();
      
      const int ibegin_srv  = this->domain->ibegin.getValue();
      const int jbegin_srv  = this->domain->jbegin.getValue();
      const int iend_srv  = this->domain->iend.getValue();
      const int jend_srv  = this->domain->jend.getValue();
      const int zoom_ni_srv = this->domain->zoom_ni_loc.getValue();
      const int zoom_nj_srv = this->domain->zoom_nj_loc.getValue();
      
      const int ibegin_zoom_srv = this->domain->zoom_ibegin_loc.getValue();
      const int jbegin_zoom_srv = this->domain->zoom_jbegin_loc.getValue();
       const int iend_zoom_srv = ibegin_zoom_srv + zoom_ni_srv-1 ;
      const int  jend_zoom_srv = jbegin_zoom_srv + zoom_nj_srv-1 ;
        
      StdSize dn = 0;      
      for (StdSize j = 1; j < this->out_i_index.size(); j++)
         dn += this->out_i_index[j]->size();
         
      ARRAY_CREATE(storeIndex_srv , int, 1, [dn]);
      ARRAY_CREATE(out_i_index_srv, int, 1, [dn]);
      ARRAY_CREATE(out_j_index_srv, int, 1, [dn]);
      ARRAY_CREATE(out_l_index_srv, int, 1, [dn]);
      
      for (StdSize i = 0, dn = 0; i < ibegin.size(); i++)
      {
         ARRAY(int, 1) storeIndex_cl   =  this->storeIndex [i + 1];
         ARRAY(int, 1) out_i_index_cl  =  this->out_i_index[i + 1];
         ARRAY(int, 1) out_j_index_cl  =  this->out_j_index[i + 1];
         ARRAY(int, 1) out_l_index_cl  =  this->out_l_index[i + 1];
                 
         int ibegin_zoom_cl = ibegin[i]; //ibegin_zoom[i];
         int jbegin_zoom_cl = jbegin[i]; //jbegin_zoom[i];
         int iend_zoom_cl = iend[i]; //ibegin_zoom[i];
         int jend_zoom_cl = jend[i]; //jbegin_zoom[i];

         int ibegin_cl = ibegin[i]; //ibegin[i];
         int jbegin_cl = jbegin[i]; //jbegin[i];
         int iend_cl = iend[i]; //ibegin[i];
         int jend_cl = jend[i]; //jbegin[i];
         
         if (ibegin_zoom.size() != 0)
         {
            ibegin_zoom_cl = ibegin_zoom[i];
            jbegin_zoom_cl = jbegin_zoom[i];
            iend_zoom_cl = ibegin_zoom[i]+ni_zoom[i]-1;
            jend_zoom_cl = jbegin_zoom[i]+nj_zoom[i]-1;
         }
         

         if (comm::CMPIManager::IsClient())
         {
           for (StdSize n = dn, m = 0; n < (dn + storeIndex_cl->size()); n++, m++)
           {
              (*storeIndex_srv)[n]  = (*storeIndex_cl)[m]; // Faux mais inutile dans le cas serveur.

              (*out_i_index_srv)[n] = (*out_i_index_cl)[m] + ibegin_cl - 1 - (ibegin_srv + ibegin_zoom_srv - 1) + 1 ;
              (*out_j_index_srv)[n] = (*out_j_index_cl)[m] + jbegin_cl - 1 - (jbegin_srv + jbegin_zoom_srv - 1) + 1 ;
              (*out_l_index_srv)[n] = (*out_l_index_cl)[m];
           }
         }
         else
         {
           for (StdSize n = dn, m = 0; n < (dn + storeIndex_cl->size()); n++, m++)
           {
              (*storeIndex_srv)[n]  = (*storeIndex_cl)[m]; // Faux mais inutile dans le cas serveur.
            (*out_i_index_srv)[n] = (*out_i_index_cl)[m] 
                                   + (ibegin_cl - 1) - (ibegin_srv - 1) + (ibegin_zoom_cl - 1) - (ibegin_zoom_srv - 1);
            (*out_j_index_srv)[n] = (*out_j_index_cl)[m]
                                   + (jbegin_cl - 1) - (jbegin_srv - 1) + (jbegin_zoom_cl - 1) - (jbegin_zoom_srv - 1);
            (*out_l_index_srv)[n] = (*out_l_index_cl)[m];
           }         
            
         }
                  
         dn += storeIndex_cl->size(); 
      }
      
      if (storeIndex_srv->size() != 0)
      {
         const int ibegin_t = 
            *std::min_element(out_i_index_srv->begin(), out_i_index_srv->end());
         const int iend_t   =
            *std::max_element(out_i_index_srv->begin(), out_i_index_srv->end());
         const int jbegin_t =   
            *std::min_element(out_j_index_srv->begin(), out_j_index_srv->end());
         const int jend_t   =
            *std::max_element(out_j_index_srv->begin(), out_j_index_srv->end());
               
                                 
         if ((ibegin_t < 0) || (jbegin_t < 0) ||
             (iend_t >= zoom_ni_srv) || (jend_t >= zoom_nj_srv))
         {
            ERROR("CGrid::computeIndexServer(void)",
                  << "[ grille = "      << this->getId()
                  << ", ibegin_t = "    << ibegin_t
                  << ", jbegin_t = "    << jbegin_t
                  << ", iend_t = "      << iend_t
                  << ", jend_t = "      << jend_t
                  << ", zoom_ni_srv = " << zoom_ni_srv
                  << ", zoom_nj_srv = " << zoom_nj_srv
                  << ", nb subdomain = "   << out_i_index.size()-1
                  <<" ] Erreur d'indexation de la grille au niveau du serveur") ;
         }
      }

      if (storeIndex_srv->size() != 0)
         for (StdSize u = 0; u < storeIndex_srv->size(); u++)
            (*local_mask)[(*out_i_index_srv)[u]][(*out_j_index_srv)[u]] = 1;

//      StdOFStream ofs(("log_server_"+this->getId()).c_str());
//      for (StdSize h = 0; h < storeIndex_srv->size(); h++)
//      {
//        ofs << "(" << (*storeIndex_srv)[h]  << ";"
//            << (*out_i_index_srv)[h] << ","
//            << (*out_j_index_srv)[h] << ","
//            << (*out_l_index_srv)[h] << ")" << std::endl;
//      }
//       ofs.close();
   
      this->storeIndex [0] = storeIndex_srv ;
      this->out_i_index[0] = out_i_index_srv;
      this->out_j_index[0] = out_j_index_srv;
      this->out_l_index[0] = out_l_index_srv;      
   }
*/
   
   void CGrid::inputFieldServer
         (const std::deque<ARRAY(double, 1)> storedClient,
                           ARRAY(double, 1)  storedServer) const
   {
      if ((this->storeIndex.size()-1 ) != storedClient.size())
         ERROR("CGrid::inputFieldServer(...)",
                << "[ Nombre de tableau attendu = " << (this->storeIndex.size()-1) << ", "
                << "[ Nombre de tableau reçu = "    << storedClient.size() << "] "
                << "Les données d'un client sont manquantes !") ;
      if (storedServer.get() != NULL)
         storedServer->resize(boost::extents[this->storeIndex[0]->num_elements()]);
      else 
         ARRAY_ASSIGN(storedServer, double, 1, [this->storeIndex[0]->num_elements()]);
         
      for (StdSize i = 0, n = 0; i < storedClient.size(); i++)
         for (StdSize j = 0; j < storedClient[i]->num_elements(); j++)
            (*storedServer)[n++] = (*storedClient[i])[j];
   }

   void CGrid::outputFieldToServer(ARRAY(double, 1) fieldIn, int rank, ARRAY(double, 1) fieldOut)
   {
     ARRAY(int,1) index=storeIndex_toSrv[rank] ;
     int nb=index->num_elements() ;
     fieldOut->resize(extents[nb]) ;
     
     for(int k=0;k<nb;k++) (*fieldOut)[k]=(*fieldIn)[(*index)[k]] ;
    }
   ///---------------------------------------------------------------

} // namespace xios
