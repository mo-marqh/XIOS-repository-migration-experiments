#ifndef __XIOS_CAxis__
#define __XIOS_CAxis__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "group_factory.hpp"
#include "virtual_node.hpp"

#include "declare_group.hpp"
#include "declare_ref_func.hpp"
#include "declare_virtual_node.hpp"
#include "attribute_array.hpp"
#include "attribute_enum.hpp"
#include "attribute_enum_impl.hpp"
#include "server_distribution_description.hpp"
#include "transformation.hpp"
#include "transformation_enum.hpp"
#include "inverse_axis.hpp"
#include "zoom_axis.hpp"

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
         typedef CTransformation<CAxis>::TransformationMapTypes TransMapTypes;

      public:
         /// Constructeurs ///
         CAxis(void);
         explicit CAxis(const StdString & id);
         CAxis(const CAxis & axis);       // Not implemented yet.
         CAxis(const CAxis * const axis); // Not implemented yet.

         static CAxis* createAxis();

         /// Accesseurs ///
         const std::set<StdString> & getRelFiles(void) const;

         /// Test ///
         bool IsWritten(const StdString & filename) const;
         bool isDistributed(void) const;

         /// Mutateur ///
         void addRelFile(const StdString & filename);

         /// Vérifications ///
         void checkAttributes(void);

         /// Destructeur ///
         virtual ~CAxis(void);

         virtual void parse(xml::CXMLNode & node);

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);
         static ENodeType GetType(void);

         void sendServerAttribut(const std::vector<int>& globalDim, int orderPositionInGrid,
                                 CServerDistributionDescription::ServerDistributionType distType);
         static bool dispatchEvent(CEventServer& event);
         static void recvServerAttribut(CEventServer& event);
         void recvServerAttribut(CBufferIn& buffer) ;
         void checkAttributesOnClient(const std::vector<int>& globalDim, int orderPositionInGrid,
                                      CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);
         void sendCheckedAttributes(const std::vector<int>& globalDim, int orderPositionInGrid,
                                    CServerDistributionDescription::ServerDistributionType disType = CServerDistributionDescription::BAND_DISTRIBUTION);

         bool hasTransformation();
         void solveInheritanceTransformation();
         TransMapTypes getAllTransformations();

      public:
        int zoom_begin_srv, zoom_end_srv, zoom_size_srv;
        int ni_srv, begin_srv, end_srv;
        int global_zoom_begin, global_zoom_size;  // The real global zoom begin and zoom size after axis is transformed (zoomed)
      private :
         void checkData();
         void checkMask();
         void checkZoom();
         void checkTransformations();
         void computeServerIndex(const std::vector<int>& globalDim, int orderPositionInGrid,
                                 CServerDistributionDescription::ServerDistributionType disType);


         void setTransformations(const TransMapTypes&);
      private:
         bool isChecked;
         bool areClientAttributesChecked_;
         std::set<StdString> relFiles;
         TransMapTypes transformationMap_;
         bool isDistributed_;

         DECLARE_REF_FUNC(Axis,axis)
   }; // class CAxis

   ///--------------------------------------------------------------

   // Declare/Define CAxisGroup and CAxisDefinition
   DECLARE_GROUP(CAxis);
} // namespace xios

#endif // __XIOS_CAxis__
