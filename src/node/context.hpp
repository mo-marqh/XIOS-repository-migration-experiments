#ifndef __XMLIO_CContext__
#define __XMLIO_CContext__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "node_type.hpp"
#include "calendar.hpp"

#include "declare_group.hpp"
//#include "context_client.hpp"
//#include "context_server.hpp"
#include "data_output.hpp"

#include <mpi.h>

namespace xmlioserver {
namespace data {
    class CDataTreatment;
} // namespace tree
} // namespace xmlioserver

namespace xmlioserver {
   class CContextClient ;
   class CContextServer ;
   
namespace tree {
   
   /// ////////////////////// Déclarations ////////////////////// ///
   class CContextGroup;
   class CContextAttributes;
   class CContext;
  
   ///--------------------------------------------------------------

   // Declare/Define CFileAttribute
   BEGIN_DECLARE_ATTRIBUTE_MAP(CContext)
#  include "context_attribute.conf"
   END_DECLARE_ATTRIBUTE_MAP(CContext)

   ///--------------------------------------------------------------

   class CContext
      : public CObjectTemplate<CContext>
      , public CContextAttributes
   {
         public :
         enum EEventId
         {
           EVENT_ID_CLOSE_DEFINITION,EVENT_ID_UPDATE_CALENDAR,
           EVENT_ID_CREATE_FILE_HEADER,EVENT_ID_CONTEXT_FINALIZE
         } ;
         
         /// typedef ///
         typedef CObjectTemplate<CContext>   SuperClass;
         typedef CContextAttributes SuperClassAttribute;

      public :

         typedef CContextAttributes RelAttributes;
         typedef CContext           RelGroup;

         //---------------------------------------------------------

      public :

         /// Constructeurs ///
         CContext(void);
         explicit CContext(const StdString & id);
         CContext(const CContext & context);       // Not implemented yet.
         CContext(const CContext * const context); // Not implemented yet.

         /// Destructeur ///
         virtual ~CContext(void);

         //---------------------------------------------------------

      public :
      
         /// Mutateurs ///
         void setCalendar(boost::shared_ptr<date::CCalendar> newCalendar);
         void setDataTreatment(boost::shared_ptr<data::CDataTreatment> datat);
      
         /// Accesseurs ///
         boost::shared_ptr<date::CCalendar>      getCalendar(void) const;
         boost::shared_ptr<data::CDataTreatment> getDataTreatment(void) const;

         /// Accesseurs statiques ///
         static StdString GetName(void);
         static StdString GetDefName(void);         
         static ENodeType GetType(void);         

         static boost::shared_ptr<CContextGroup> GetContextGroup(void);

      public :

         /// Traitements ///
         virtual void solveDescInheritance(const CAttributeMap * const parent = 0);
         void solveFieldRefInheritance(void);
         void solveCalendar(void);

         /// Autres méthodes statiques ///
         static void ShowTree(StdOStream & out = std::clog);
         static void CleanTree(void);

         /// Test ///
         virtual bool hasChild(void) const;

         bool eventLoop(void) ;
         bool serverLoop(void) ;
         void clientLoop(void) ;
         void initServer(MPI_Comm intraComm, MPI_Comm interComm) ;
         void initClient(MPI_Comm intraComm, MPI_Comm interComm) ;
         CContextServer* server ;
         CContextClient* client ;
         bool hasClient ;
         bool hasServer ;
         void finalize(void) ;
         void closeDefinition(void) ;
         void findAllEnabledFields(void);
         void solveAllGridRef(void);
         void solveAllOperation(void);
         void solveAllInheritance(void) ;
         void findEnabledFiles(void);
         void closeAllFile(void) ;
         void updateCalendar(int step) ;
         void createFileHeader(void ) ;
      // dispatch event
         static bool dispatchEvent(CEventServer& event) ;
         void sendCloseDefinition(void) ;
         void sendUpdateCalendar(int step) ;
         void sendCreateFileHeader(void) ;
         static void recvUpdateCalendar(CEventServer& event) ;
         void recvUpdateCalendar(CBufferIn& buffer) ;
         static void recvCloseDefinition(CEventServer& event) ;
         static void recvCreateFileHeader(CEventServer& event) ;
         void recvCreateFileHeader(CBufferIn& buffer) ;
         static shared_ptr<CContext> current(void) ;
         
      public :
      
         /// Autres ///
         virtual void parse(xml::CXMLNode & node);

         virtual StdString toString(void) const;
         virtual void toBinary  (StdOStream & os) const;
         virtual void fromBinary(StdIStream & is);
         
      public :
      
         boost::shared_ptr<date::CCalendar>      calendar;
         boost::shared_ptr<data::CDataTreatment> datat;
 
         std::vector<boost::shared_ptr<CFile> > enabledFiles;


   }; // class CContext

   ///--------------------------------------------------------------

   // Declare/Define CContextGroup and CContextDefinition
   DECLARE_GROUP(CContext);

   ///--------------------------------------------------------------

} // namespace tree
} // namespace xmlioserver

#endif // __XMLIO_CContext__
