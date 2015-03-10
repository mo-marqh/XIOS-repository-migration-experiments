#include "axis.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "message.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "xmlioserver_spl.hpp"

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CAxis::CAxis(void)
      : CObjectTemplate<CAxis>()
      , CAxisAttributes(), isChecked(false), relFiles(), baseRefObject(), areClientAttributesChecked_(false)
   { /* Ne rien faire de plus */ }

   CAxis::CAxis(const StdString & id)
      : CObjectTemplate<CAxis>(id)
      , CAxisAttributes(), isChecked(false), relFiles(), baseRefObject(), areClientAttributesChecked_(false)
   { /* Ne rien faire de plus */ }

   CAxis::~CAxis(void)
   { /* Ne rien faire de plus */ }

   ///---------------------------------------------------------------

   const std::set<StdString> & CAxis::getRelFiles(void) const
   {
      return (this->relFiles);
   }

   bool CAxis::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   void CAxis::addRelFile(const StdString & filename)
   {
      this->relFiles.insert(filename);
   }

   //----------------------------------------------------------------

   StdString CAxis::GetName(void)   { return (StdString("axis")); }
   StdString CAxis::GetDefName(void){ return (CAxis::GetName()); }
   ENodeType CAxis::GetType(void)   { return (eAxis); }

   //----------------------------------------------------------------

   void CAxis::checkAttributes(void)
   {
      if (this->size.isEmpty())
         ERROR("CAxis::checkAttributes(void)",
               << "Attribute <size> of the axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] must be specified");
      StdSize size = this->size.getValue();

      if (!this->ibegin.isEmpty())
      {
        StdSize ibegin = this->ibegin.getValue();
        if ((ibegin < 0) || (ibegin > size-1))
          ERROR("CAxis::checkAttributes(void)",
                << "Attribute <ibegin> of the axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] must be non-negative and smaller than size-1");
      }
      else this->ibegin.setValue(0);

      if (!this->ni.isEmpty())
      {
        StdSize ni = this->ni.getValue();
        if ((ni < 0) || (ni > size))
          ERROR("CAxis::checkAttributes(void)",
                << "Attribute <ni> of the axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] must be non-negative and smaller than size");
      }
      else this->ni.setValue(size);

              << "One or more attributes among <zoom_begin>, <zoom_end>, <zoom_size> of axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] are not well specified");
      StdSize true_size = value.numElements();
      if (size != true_size)
         ERROR("CAxis::checkAttributes(void)",
               << "The array \'value\' of axis [ id = '" << getId() << "' , context = '" << CObjectFactory::GetCurrentContextId() << "' ] has a different size that the one defined by the \'size\' attribute");

      this->checkData();
      this->checkMask();
      this->checkZoom();
   }

   void CAxis::checkData()
   {
      if (data_begin.isEmpty()) data_begin.setValue(0);
      if (!data_n.isEmpty() && data_n.getValue() <= 0)
      {
        ERROR("CAxis::checkData(void)",
              << "Data dimension is negative (data_n).");
      }
      else if (data_n.isEmpty())
        data_n.setValue(ni.getValue());

      if (data_index.isEmpty())
      {
        int dn = data_n.getValue();
        data_index.resize(dn);
        for (int i = 0; i < dn; ++i) data_index(i) = (i+1);
      }
   }

   void CAxis::checkZoom(void)
   {
      StdSize zoom_begin,zoom_end, zoom_size, axisSize;

      zoom_begin = (this->zoom_begin.isEmpty()) ?  0 : this->zoom_begin.getValue() ;
      zoom_size  = (this->zoom_size.isEmpty()) ?  size.getValue() : this->zoom_size.getValue() ;
      zoom_end   = (this->zoom_end.isEmpty()) ?  (size.getValue() - 1) : this->zoom_end.getValue() ;

      if (this->zoom_begin.isEmpty()) zoom_begin=zoom_end-zoom_size+1 ;
      if (this->zoom_end.isEmpty()) zoom_end=zoom_begin+zoom_size-1 ;
      if (this->zoom_size.isEmpty()) zoom_size=zoom_end-zoom_begin+1 ;
      axisSize = size.getValue();

      if ( (zoom_begin < 0) || (zoom_begin > axisSize-1) || (zoom_end<0) || (zoom_end>axisSize-1) || (zoom_size<1) || (zoom_size>axisSize) || (zoom_begin>zoom_end))
        ERROR("CAxis::checkAttributes(void)",<< "One or more attribut of <zoom_begin>, <zoom_end>, <zoom_size>, are not well specified") ;

      this->zoom_begin.setValue(zoom_begin) ;
      this->zoom_end.setValue(zoom_end) ;
      this->zoom_size.setValue(zoom_size) ;
   }

   void CAxis::checkMask()
   {
      int begin_mask = 0,
          end_mask = ni.getValue()-1;

      if (!zoom_begin.isEmpty())
      {
         int zoom_end = zoom_begin.getValue() + zoom_size.getValue() - 1;

         begin_mask = std::max(ibegin.getValue(), zoom_begin.getValue());
         end_mask   = std::min(ibegin.getValue() + ni.getValue()-1, zoom_end);

         begin_mask -= ibegin.getValue();
         end_mask   -= ibegin.getValue();
      }


      if (!mask.isEmpty())
      {
         if (mask.extent(0) != ni)
            ERROR("CAxis::checkMask(void)",
                  <<"the mask has not the same size than the local axis"<<endl
                  <<"Local size is "<<ni<<"x"<<endl
                  <<"Mask size is "<<mask.extent(0)<<"x");
         for (int i = 0; i < ni; ++i)
         {
           if (i < begin_mask && i > end_mask)  mask(i) = false;
         }
      }
      else // (!mask.hasValue())
      { // Si aucun masque n'est défini,
        // on en crée un nouveau qui valide l'intégralité du domaine.
         mask.resize(ni);
         for (int i = 0; i < ni.getValue(); ++i)
         {
               if (i >= begin_mask && i <= end_mask)
                 mask(i) = true;
               else  mask(i) = false;
         }
      }
   }

  bool CAxis::dispatchEvent(CEventServer& event)
   {
      if (SuperClass::dispatchEvent(event)) return true ;
      else
      {
        switch(event.type)
        {
           case EVENT_ID_SERVER_ATTRIBUT :
             recvServerAttribut(event) ;
             return true ;
             break ;
           default :
             ERROR("bool CContext::dispatchEvent(CEventServer& event)",
                    <<"Unknown Event") ;
           return false ;
         }
      }
   }

   void CAxis::checkAttributesOnClient(const std::vector<int>& globalDim, int orderPositionInGrid,
                                       CServerDistributionDescription::ServerDistributionType distType)
   {
     if (this->areClientAttributesChecked_) return;
     this->checkAttributes();

     CContext* context=CContext::getCurrent() ;
     if (context->hasClient)
     {
       computeServerIndex(globalDim, orderPositionInGrid, distType);
     }

     this->areClientAttributesChecked_ = true;
   }

   void CAxis::computeServerIndex(const std::vector<int>& globalDim, int orderPositionInGrid,
                                  CServerDistributionDescription::ServerDistributionType distType)
   {
     CServerDistributionDescription serverDescription(globalDim);

     CContext* context=CContext::getCurrent() ;
     CContextClient* client=context->client ;
     int nbServer=client->serverSize ;
     int serverRank=client->getServerLeader() ;

     serverDescription.computeServerDistribution(nbServer, false, distType);
     std::vector<std::vector<int> > serverIndexBegin = serverDescription.getServerIndexBegin();
     std::vector<std::vector<int> > serverDimensionSizes = serverDescription.getServerDimensionSizes();
     begin_srv = (serverIndexBegin[serverRank])[orderPositionInGrid];
     ni_srv = serverDimensionSizes[serverRank][orderPositionInGrid];
     end_srv = begin_srv+ni_srv-1;
   }

   // Send all checked attributes to server
   void CAxis::sendCheckedAttributes(const std::vector<int>& globalDim, int orderPositionInGrid,
                                     CServerDistributionDescription::ServerDistributionType distType)
   {
     if (!this->areClientAttributesChecked_) checkAttributesOnClient(globalDim,
                                                                     orderPositionInGrid,
                                                                     distType);
     CContext* context=CContext::getCurrent() ;

     if (this->isChecked) return;
     if (context->hasClient)
     {
       sendServerAttribut() ;
     }

     this->isChecked = true;
   }

  void CAxis::sendServerAttribut(void)
  {
    CContext* context=CContext::getCurrent();
    CContextClient* client=context->client;

    CEventClient event(getType(),EVENT_ID_SERVER_ATTRIBUT) ;
    if (client->isServerLeader())
    {
      CMessage msg ;
      msg<<this->getId() ;
      msg<<ni_srv<<begin_srv<<end_srv;
      event.push(client->getServerLeader(),1,msg) ;
      client->sendEvent(event) ;
    }
    else client->sendEvent(event) ;
  }

  void CAxis::recvServerAttribut(CEventServer& event)
  {
    CBufferIn* buffer=event.subEvents.begin()->buffer;
    string axisId ;
    *buffer>>axisId ;
    get(axisId)->recvServerAttribut(*buffer) ;
  }

  void CAxis::recvServerAttribut(CBufferIn& buffer)
  {
    int zoom_end = zoom_begin.getValue()+zoom_size.getValue()-1;
    int ni_srv, begin_srv, end_srv;

    buffer>>ni_srv>>begin_srv>>end_srv;

    zoom_begin_srv = zoom_begin.getValue() > begin_srv ? zoom_begin.getValue() : begin_srv ;
    zoom_end_srv   = zoom_end < end_srv ? zoom_end : end_srv ;
    zoom_size_srv  = zoom_end_srv-zoom_begin_srv+1 ;

    if (zoom_size_srv<=0)
    {
      zoom_begin_srv=0 ; zoom_end_srv=0 ; zoom_size_srv=0 ;
    }
  }

   DEFINE_REF_FUNC(Axis,axis)

   ///---------------------------------------------------------------

} // namespace xios
