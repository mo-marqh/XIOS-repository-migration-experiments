#ifndef __XMLIO_CRebuild__
#define __XMLIO_CRebuild__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"

namespace xmlioserver
{
   namespace io
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CRebuild
      {
         public :

            /// Construteurs ///
            CRebuild(void);                            // Never implemented.
            explicit CRebuild(const StdString & filename);
            CRebuild(const CRebuild  & rebuild);       // Never implemented.
            CRebuild(const CRebuild  * const rebuild); // Never implemented.

            /// Destructeur ///
            virtual ~CRebuild(void);

      }; // class CRebuild

   } // namespace io
} // namespace xmlioserver

#endif // __XMLIO_CRebuild__
