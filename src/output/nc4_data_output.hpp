#ifndef __XMLIO_NC4_DATA_OUTPUT__
#define __XMLIO_NC4_DATA_OUTPUT__

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "onetcdf4.hpp"
#include "data_output.hpp"

namespace xios
{
      /// ////////////////////// Déclarations ////////////////////// ///

      class CNc4DataOutput
         : protected CONetCDF4
         , public virtual CDataOutput
      {
         public :

            /// Définition de type ///
            typedef CONetCDF4   SuperClassWriter;
            typedef CDataOutput SuperClass;

            /// Constructeurs ///
            CNc4DataOutput
               (const StdString & filename, bool exist);
            CNc4DataOutput
               (const StdString & filename, bool exist, MPI_Comm comm_server, bool multifile, bool isCollective=true);

            CNc4DataOutput(const CNc4DataOutput & dataoutput);       // Not implemented.
            CNc4DataOutput(const CNc4DataOutput * const dataoutput); // Not implemented.

            /// Accesseur ///
            const StdString & getFileName(void) const;

            /// Destructeur ///
            virtual ~CNc4DataOutput(void);
            bool singleDomain ;
            bool isCollective ;
         protected :

            /// Ecriture ///
            virtual void writeDomain_    (const boost::shared_ptr<tree::CDomain>   domain);
            virtual void writeAxis_      (const boost::shared_ptr<tree::CAxis>     axis);
            virtual void writeField_     (const boost::shared_ptr<tree::CField>    field);
            virtual void writeFieldData_ (const boost::shared_ptr<tree::CField>    field);
            virtual void writeFile_      (const boost::shared_ptr<tree::CFile>     file);
            virtual void closeFile_      (void);
            virtual void syncFile_      (void);
            virtual void writeTimeAxis_  (const boost::shared_ptr<tree::CField>    field,
                                          const boost::shared_ptr<date::CCalendar> cal);

         protected :
         
            void writeLocalAttributes(int ibegin, int ni, int jbegin, int nj, StdString domid);

            void writeTimeAxisAttributes(const StdString & axis_name,
                                         const StdString & calendar,
                                         const StdString & units,
                                         const StdString & time_origin,
                                         const StdString & standard_name = StdString("time"),
                                         const StdString & long_name     = StdString("Time axis"),
                                         const StdString & title         = StdString("Time"));

            void writeFileAttributes(const StdString & name,
                                     const StdString & description,
                                     const StdString & conventions,
                                     const StdString & production,
                                     const StdString & timeStamp);

            void writeMaskAttributes(const StdString & mask_name,
                                     int data_dim,
                                     int data_ni     = 0,
                                     int data_nj     = 0,
                                     int data_ibegin = 0,
                                     int data_jbegin = 0);

            void writeAxisAttributes(const StdString & axis_name,
                                     const StdString & axis,
                                     const StdString & standard_name,
                                     const StdString & long_name,
                                     const StdString & units,
                                     const StdString & nav_model);
            template <class T>
               void writeAxisData(const StdString & axis_name,
                                  const ARRAY(T, 1) data,
                                  bool collective, StdSize record,
                                  const std::vector<StdSize> * start = NULL,
                                  const std::vector<StdSize> * count = NULL);

         private :

            /// Traitement ///
            StdString getTimeStamp(void) const;

            /// Propriétés privées ///
            MPI_Comm comm_server;
            const StdString filename;

      }; // class CNc4DataOutput

} // namespace xios

#endif //__XMLIO_NC4_DATA_OUTPUT__
