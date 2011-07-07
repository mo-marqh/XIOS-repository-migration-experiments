#ifndef __XMLIO_INETCDF4__
#define __XMLIO_INETCDF4__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "array.hpp"
#include "data_output.hpp"
#include "mpi_manager.hpp"

#include <mpi.h>
#define MPI_INCLUDED
#include <netcdf.h>
extern "C" {
#include <netcdf_par.h>
}


#ifndef UNLIMITED_DIM
   #define UNLIMITED_DIM (size_t)(-1)
#endif  //UNLIMITED_DIM

namespace xmlioserver
{
   namespace io
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CONetCDF4
         : public virtual CDataOutput
      {
         public :

            /// Définition de type ///
            typedef std::vector<StdString> CONetCDF4Path;

            /// Constructeurs ///
            CONetCDF4(const StdString & filename, bool exist, const MPI_Comm * comm = NULL);
            CONetCDF4(const StdString & filename, bool exist, const comm::MPIComm * comm, bool);

            CONetCDF4(const CONetCDF4 & onetcdf4);       // Not implemented.
            CONetCDF4(const CONetCDF4 * const onetcdf4); // Not implemented.


            /// Initialisation ///
            void initialize(const StdString & filename, bool exist, const MPI_Comm * comm);
            void definition_start(void);
            void definition_end(void);

            /// Mutateurs ///
            void setCurrentPath(const CONetCDF4Path & path);

            int addGroup(const StdString & name);
            int addDimension(const StdString& name, const StdSize size = UNLIMITED_DIM);
            int addVariable(const StdString & name, nc_type type,
                            const std::vector<StdString> & dim);
                            
      //----------------------------------------------------------------
         public :
         
            template <class T>
               void setDefaultValue(const StdString & varname, const T * value = NULL);
         
            template <class T>
               void addAttribute
                  (const StdString & name, const T & value, const StdString * varname = NULL);

            /// Ecriture des données ///
            template <class T, StdSize ndim>
               void writeData(const ARRAY(T, ndim) data, const StdString & name,
                              bool collective, StdSize record,
                              const std::vector<StdSize> * start = NULL,
                              const std::vector<StdSize> * count = NULL);

            void writeData(const ARRAY(int, 2) data, const StdString & name);

            /// Accesseur ///
            const CONetCDF4Path & getCurrentPath(void) const;

            /// Destructeur ///
            virtual ~CONetCDF4(void);
            
      //----------------------------------------------------------------
      
         protected :

            /// Ecriture ///
            virtual void writeField_ (const boost::shared_ptr<tree::CField>  field)  = 0;
            virtual void writeDomain_(const boost::shared_ptr<tree::CDomain> domain) = 0;
            virtual void writeAxis_  (const boost::shared_ptr<tree::CAxis>   axis)   = 0;

            /// Accesseurs ///
            int getCurrentGroup(void);
            int getGroup(const CONetCDF4Path & path);
            int getVariable(const StdString & varname);
            int getDimension(const StdString & dimname);
            std::vector<StdSize> getDimensions(const StdString & varname);
            int getUnlimitedDimension(void);

            bool varExist(const StdString & varname);

      //----------------------------------------------------------------
      
         private :
         
            template <class T>
               void writeData_(int grpid, int varid,
                               const std::vector<StdSize> & sstart,
                               const std::vector<StdSize> & scount, T * data);

            void getWriteDataInfos(const StdString & name, StdSize record, StdSize & array_size,
                                   std::vector<StdSize> & sstart,
                                   std::vector<StdSize> & scount,
                                   const std::vector<StdSize> * start,
                                   const std::vector<StdSize> * count);

            /// Vérification des erreurs NetCDF ///
            void CheckError(int status);

            /// Propriétés privées ///
            CONetCDF4Path path;
            int ncidp;
            bool wmpi;

      }; // class CONetCDF4

      ///---------------------------------------------------------------
           
      template <class T, StdSize ndim>
         void CONetCDF4::writeData(const ARRAY(T, ndim) data, const StdString & name,
                                   bool collective, StdSize record,
                                   const std::vector<StdSize> * start,
                                   const std::vector<StdSize> * count)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);
         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;

         if (this->wmpi && collective)
            CheckError(nc_var_par_access(grpid, varid, NC_COLLECTIVE));
         if (this->wmpi && !collective)
            CheckError(nc_var_par_access(grpid, varid, NC_INDEPENDENT));

         this->getWriteDataInfos
         (name, record, array_size,  sstart, scount, start, count);
         this->writeData_(grpid, varid, sstart, scount, data->data());
      }
      
      //----------------------------------------------------------------
           
      template <class T>
         void CONetCDF4::setDefaultValue(const StdString & varname, const T * value)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(varname);
         
         if (value != NULL)
         {
            CheckError(nc_def_var_fill(grpid, varid, 0, value));
            this->addAttribute(StdString("missing_value"), *value, &varname);
         }
         else
            CheckError(nc_def_var_fill(grpid, varid, 1, NULL));         
      }
     
      ///---------------------------------------------------------------

   } // namespace io
} // namespace xmlioserver

#endif //__XMLIO_INETCDF4__
