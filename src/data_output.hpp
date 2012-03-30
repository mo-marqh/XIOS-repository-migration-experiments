#ifndef __XMLIO_DATA_OUTPUT__
#define __XMLIO_DATA_OUTPUT__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "grid.hpp"
#include "field.hpp"

#include <mpi.h>
#define MPI_INCLUDED
#include <netcdf.h>

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///
      class CDataOutput
      {
         public :

            /// Définition de type ///
            typedef enum { ONE_FILE = 0, MULTI_GROUP, MULTI_FILE } EDataOutputType;

            /// Ecriture ///
            void writeFile     (const boost::shared_ptr<CFile>  file);
            void syncFile     (void);
            void closeFile     (void);
            void writeField    (const boost::shared_ptr<CField> field);
            void writeFieldGrid(const boost::shared_ptr<CField> field);
            void writeFieldData(const boost::shared_ptr<CField> field);

            virtual void definition_start(void) = 0;
            virtual void definition_end(void)   = 0;

            virtual ~CDataOutput(void);

         protected:

            /// Ecriture ///
            void writeGrid(const boost::shared_ptr<CGrid>   grid);
            void writeGrid(const boost::shared_ptr<CDomain> domain,
                           const boost::shared_ptr<CAxis>   axis);
            void writeGrid(const boost::shared_ptr<CDomain> domain);

            virtual void writeFile_       (const boost::shared_ptr<CFile>     file)   = 0;
            virtual void closeFile_       (void)                                            = 0;
            virtual void syncFile_       (void)                                            = 0;
            virtual void writeField_      (const boost::shared_ptr<CField>    field)  = 0;
            virtual void writeFieldData_  (const boost::shared_ptr<CField>    field)  = 0;
            virtual void writeDomain_     (const boost::shared_ptr<CDomain>   domain) = 0;
            virtual void writeAxis_       (const boost::shared_ptr<CAxis>     axis)   = 0;
            virtual void writeTimeAxis_   (const boost::shared_ptr<CField>    field,
                                           const boost::shared_ptr<CCalendar> cal)    = 0;

            /// Propriétés protégées ///
            EDataOutputType type;

      }; // class CDataOutput

} // namespace xios

#endif //__XMLIO_DATA_OUTPUT__
