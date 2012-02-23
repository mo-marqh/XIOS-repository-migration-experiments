#ifndef __XMLIO_DATA_OUTPUT__
#define __XMLIO_DATA_OUTPUT__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "grid.hpp"
#include "field.hpp"

#include <mpi.h>
#include <netcdf.h>

namespace xmlioserver
{
   namespace io
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CDataOutput
      {
         public :

            /// Définition de type ///
            typedef enum { ONE_FILE = 0, MULTI_GROUP, MULTI_FILE } EDataOutputType;

            /// Ecriture ///
            void writeFile     (const boost::shared_ptr<tree::CFile>  file);
            void syncFile     (void);
            void closeFile     (void);
            void writeField    (const boost::shared_ptr<tree::CField> field);
            void writeFieldGrid(const boost::shared_ptr<tree::CField> field);
            void writeFieldData(const boost::shared_ptr<tree::CField> field);

            virtual void definition_start(void) = 0;
            virtual void definition_end(void)   = 0;

            virtual ~CDataOutput(void);

         protected:

            /// Ecriture ///
            void writeGrid(const boost::shared_ptr<tree::CGrid>   grid);
            void writeGrid(const boost::shared_ptr<tree::CDomain> domain,
                           const boost::shared_ptr<tree::CAxis>   axis);
            void writeGrid(const boost::shared_ptr<tree::CDomain> domain);

            virtual void writeFile_       (const boost::shared_ptr<tree::CFile>     file)   = 0;
            virtual void closeFile_       (void)                                            = 0;
            virtual void syncFile_       (void)                                            = 0;
            virtual void writeField_      (const boost::shared_ptr<tree::CField>    field)  = 0;
            virtual void writeFieldData_  (const boost::shared_ptr<tree::CField>    field)  = 0;
            virtual void writeDomain_     (const boost::shared_ptr<tree::CDomain>   domain) = 0;
            virtual void writeAxis_       (const boost::shared_ptr<tree::CAxis>     axis)   = 0;
            virtual void writeTimeAxis_   (const boost::shared_ptr<tree::CField>    field,
                                           const boost::shared_ptr<date::CCalendar> cal)    = 0;

            /// Propriétés protégées ///
            EDataOutputType type;

      }; // class CDataOutput

   } // namespace io
} // namespace xmlioserver

#endif //__XMLIO_DATA_OUTPUT__
