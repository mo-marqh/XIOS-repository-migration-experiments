#ifndef __SERVER_HPP__
#define __SERVER_HPP__

#include "xios_spl.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "mpi.hpp"
#include "event_scheduler.hpp"
#include "oasis_cinterface.hpp"

namespace xios
{
    class CServersRessource ;

    class CThirdPartyDriver
    {
      public:
        CThirdPartyDriver()
        {
          oasis_init(CXios::xiosCodeId);
        };
        virtual ~CThirdPartyDriver()
        {
          oasis_finalize();
        };
        virtual void endSynchronizedDefinition()
        {
          oasis_enddef();
        };
        virtual void getComponentCommunicator(MPI_Comm &intraComm)
        {
          oasis_get_localcomm(intraComm);
        };
    };
  
    class CServer
    {
      public:
        static void initialize(void);
        static void xiosGlobalCommByFileExchange(MPI_Comm serverComm) ;
        static void xiosGlobalCommByPublishing(MPI_Comm serverComm) ;

        static void listenRootOasisEnddef(void);
        static void listenOasisEnddef(void);
        static void testingEventScheduler(void);
        
        static void finalize(void);
        static void printProfile(void);
        static void eventLoop(void);
        
        static MPI_Comm intraComm_;
        static MPI_Comm serversComm_;
        static std::list<MPI_Comm> interCommLeft;           // interComm between server (primary, classical or secondary) and its client (client or primary server)
        static std::list<MPI_Comm> interCommRight;          // interComm between primary server and secondary server (non-empty only for primary server pool)
        static std::list<MPI_Comm> contextInterComms;  // list of context intercomms
        static std::list<MPI_Comm> contextIntraComms;  // list of context intercomms (needed only in case of secondary servers)
        static CEventScheduler* eventScheduler;

        static int serverLevel ;

        struct contextMessage
        {
          int nbRecv;
          int leaderRank;
        };

        static bool isRoot;

        static map<string,CContext*> contextList;
        static bool finished;
        static bool is_MPI_Initialized;

      public:
        //! Get rank of the current process in the intraComm
        static int getRank();

        //!< Get global ranks of secondary server processes
        static vector<int>& getSecondaryServerGlobalRanks();

        //! Open a file stream to write the info logs
        static void openInfoStream(const StdString& fileName);
        //! Write the info logs to standard output
        static void openInfoStream();
        //! Close the info logs file if it opens
        static void closeInfoStream();

        //! Open a file stream to write the error log
        static void openErrorStream(const StdString& fileName);
        //! Write the error log to standard error output
        static void openErrorStream();
        //! Close the error log file if it opens
        static void closeErrorStream();

        static CServersRessource* getServersRessource(void) { return serversRessource_;}
        static void launchServersRessource(MPI_Comm commServer) ;
        static void finalizeServersRessource(void) ;
        static void openStream(const StdString& fileName, const StdString& ext, std::filebuf* fb);
      
      private:
        static vector<int> sndServerGlobalRanks;  //!< Global ranks of pool leaders on the secondary server
        static int rank_;                         //!< If (!oasis) global rank, else rank in the intraComm returned by oasis
        static int nbContexts;                    //!< Number of contexts registered by server
        static StdOFStream m_infoStream;
        static StdOFStream m_errorStream;
        static CServersRessource* serversRessource_  ;

        static CThirdPartyDriver* driver_;
    };
}

#endif
