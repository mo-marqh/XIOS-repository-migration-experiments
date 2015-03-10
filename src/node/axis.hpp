#ifndef __XMLIO_CAxis__
#define __XMLIO_CAxis__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "group_factory.hpp"

#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "server_distribution_description.hpp"

namespace xios {

   /// ////////////////////// Déclarations ////////////////////// ///

   class CAxisGroup;
   class CAxisAttributes;
   class CAxis;

   ///--------------------------------------------------------------

   // Declare/Define CAxisAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CAxis)
#  include "axis_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CAxis)

   ///--------------------------------------------------------------

   class CAxis
      : public CObjectTemplate<CAxis>
      , public CAxisAttributes
   {
         enum EEventId
         {
           EVENT_ID_SERVER_ATTRIBUT
         } ;

         /// typedef ///
         typedef CObjectTemplate<CAxis>   SuperClass;
         typedef CAxisAttributes SuperClassAttribute;

      public :

         typedef CAxisAttributes RelAttributes;
         typedef CAxisGroup      RelGroup;

         /// Constructeurs ///
         CAxis(void);
         explicit CAxis(const StdString & id);
         CAxis(const CAxis & axis);       // Not implemented yet.
         CAxis(const CAxis * const axis); // Not implemented yet.

         /// Accesseurs ///
         const std::set<StdString> & getRelFiles(void) const;

         /// Test ///
         bool IsWritten(const StdString & filename) const;

         /// Mutateur ///
         void addRelFile(const StdString & filename);

         /// Vérifications ///
         void checkAttributes(void);

         /// Destructeur ///
         virtual ~CAxis(void);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         static ENodeType GetType(void);

         void sendServerAttribut(void);
         static bool dispatchEvent(CEventServer& event);
         static void recvServerAttribut(CEventServer& event);
         void recvServerAttribut(CBufferIn& buffer) ;
         void checkAttributesOnClient(const std::vector<int>& globalDim, int orderPositionInGrid,
                                      CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);
         void sendCheckedAttributes(const std::vector<int>& globalDim, int orderPositionInGrid,
                                    CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);
      public:
        int zoom_begin_srv, zoom_end_srv, zoom_size_srv;
        int ni_srv, begin_srv, end_srv;
      private :
         void checkData();
         void checkMask();
         void checkZoom();
         void computeServerIndex(const std::vector<int>& globalDim, int orderPositionInGrid,
                                 CServerDistributionDescription::ServerDistributionType disType);
      private:

         bool isChecked;
         bool areClientAttributesChecked_;
         std::set<StdString> relFiles;

         DECLARE_REF_FUNC(Axis,axis)


   }; // class CAxis

   ///--------------------------------------------------------------

   // Declare/Define CAxisGroup and CAxisDefinition
   DECLARE_GROUP(CAxis);

   ///--------------------------------------------------------------

} // namespace xios

#endif // __XMLIO_CAxis__
