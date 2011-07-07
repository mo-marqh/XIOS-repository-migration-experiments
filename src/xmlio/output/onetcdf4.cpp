#include "onetcdf4.hpp"

namespace xmlioserver
{
   namespace io
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CONetCDF4::CONetCDF4
         (const StdString & filename, bool exist, const MPI_Comm * comm)
            : path()
      {
         this->wmpi = (comm != NULL);
         this->initialize(filename, exist, comm);
      }
      
      //---------------------------------------------------------------
      
      CONetCDF4::CONetCDF4
         (const StdString & filename, bool exist, const comm::MPIComm * comm, bool)
         : path()
      {
         this->wmpi = (comm != NULL);
         MPI_Comm * null_comm = NULL;
         if (comm == NULL)
            this->initialize(filename, exist, null_comm);
         else
         {
            MPI_Comm comm_c = MPI_Comm_f2c(*comm);
            this->initialize(filename, exist, &comm_c);
         }
      }
      
      //---------------------------------------------------------------
      
      CONetCDF4::~CONetCDF4(void)
      {
         CheckError(nc_close(this->ncidp));
      }

      ///--------------------------------------------------------------

      void CONetCDF4::initialize
         (const StdString & filename, bool exist, const MPI_Comm * comm)
      {
         if (!exist)
         {
            if (comm != NULL)
            {
               CheckError(nc_create_par
                  (filename.c_str(), NC_NETCDF4|NC_MPIIO, *comm, MPI_INFO_NULL, &this->ncidp));
            }
            else CheckError(nc_create(filename.c_str(), NC_NETCDF4, &this->ncidp));
         }
         else
         {
            if (comm != NULL)
               CheckError(nc_open_par
                  (filename.c_str(), NC_NETCDF4|NC_MPIIO, *comm, MPI_INFO_NULL, &this->ncidp));
            else  CheckError(nc_open(filename.c_str(), NC_NETCDF4, &this->ncidp));
         }
      }
      
      //---------------------------------------------------------------
      
      void CONetCDF4::definition_start(void)
      { 
         CheckError(nc_redef(this->ncidp));
      }
      
      //---------------------------------------------------------------
      
      void CONetCDF4::definition_end(void)
      { 
         CheckError(nc_enddef(this->ncidp));
      }
      
      //---------------------------------------------------------------
      
      void CONetCDF4::CheckError(int status)
      {
         if (status != NC_NOERR)
         {
            StdString errormsg (nc_strerror(status)); // fuite mémoire ici ?
            ERROR("CONetCDF4::CheckError(int status)",
                  << "[ status = " << status << " ] " << errormsg);
         }
      }

      //---------------------------------------------------------------
      
      int CONetCDF4::getCurrentGroup(void)
      {
         return (this->getGroup(this->getCurrentPath()));
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getGroup(const CONetCDF4Path & path)
      {
         int retvalue = this->ncidp;
         
         CONetCDF4Path::const_iterator
            it  = path.begin(), end = path.end();

         for (;it != end; it++)
         {
            const StdString & groupid = *it;
            CheckError(nc_inq_ncid(retvalue, const_cast<char*>(groupid.c_str()), &retvalue));
         }
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getVariable(const StdString & varname)
      {
         int varid = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_inq_varid (grpid, varname.c_str(), &varid));
         return (varid);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getDimension(const StdString & dimname)
      {
         int dimid = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_inq_dimid (grpid, dimname.c_str(), &dimid));
         return (dimid);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::getUnlimitedDimension(void)
      {
         int dimid = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_inq_unlimdim (grpid, &dimid));
         return (dimid);
      }
      
      //---------------------------------------------------------------
      
      std::vector<StdSize> CONetCDF4::getDimensions(const StdString & varname)
      {
         StdSize size = 0;
         std::vector<StdSize> retvalue;
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(varname);
         int nbdim = 0, *dimid = NULL;

         CheckError(nc_inq_varndims(grpid, varid, &nbdim));
         dimid = new int[nbdim]();
         CheckError(nc_inq_vardimid(grpid, varid, dimid));

         for (int i = 0; i < nbdim; i++)
         {
            CheckError(nc_inq_dimlen (grpid, dimid[i], &size));
            if (size == NC_UNLIMITED)
                size = UNLIMITED_DIM;
            retvalue.push_back(size);
         }

         return (retvalue);
      }

      //---------------------------------------------------------------

      const CONetCDF4::CONetCDF4Path & CONetCDF4::getCurrentPath(void) const
      { return (this->path); }

      void CONetCDF4::setCurrentPath(const CONetCDF4Path & path)
      { this->path = path; }

      //---------------------------------------------------------------

      int CONetCDF4::addGroup(const StdString & name)
      {
         int retvalue = 0;
         int grpid = this->getCurrentGroup();
         CheckError(nc_def_grp(grpid, const_cast<char*>(name.c_str()), &retvalue));
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::addDimension(const StdString& name, const StdSize size)
      {
         int retvalue = 0;
         int grpid = this->getCurrentGroup();
         if (size != UNLIMITED_DIM)
            CheckError(nc_def_dim (grpid, name.c_str(), size, &retvalue));
         else
            CheckError(nc_def_dim (grpid, name.c_str(), NC_UNLIMITED, &retvalue));
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      int CONetCDF4::addVariable(const StdString & name, nc_type type,
                                  const std::vector<StdString> & dim)
      {
         int retvalue = 0;
         std::vector<int> dimids;
         int grpid = this->getCurrentGroup();
         
         std::vector<StdString>::const_iterator
            it  = dim.begin(), end = dim.end();

         for (;it != end; it++)
         {
            const StdString & dimid = *it;
            dimids.push_back(this->getDimension(dimid));
         }
         CheckError(nc_def_var (grpid, name.c_str(), type, dimids.size(), &(dimids[0]), &retvalue));
         return (retvalue);
      }

      //---------------------------------------------------------------

      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const StdString & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att(grpid, varid, name.c_str(), NC_CHAR, value.size()+1, value.c_str()));
         //CheckError(nc_put_att_string(grpid, varid, name.c_str(), 1, &str));
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const double & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_double(grpid, varid, name.c_str(), NC_DOUBLE,1, &value));
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const float & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_float(grpid, varid, name.c_str(), NC_FLOAT, 1, &value));
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::addAttribute
            (const StdString & name, const int & value, const StdString * varname )
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CheckError(nc_put_att_int(grpid, varid, name.c_str(), NC_INT,1, &value));
      }

      //---------------------------------------------------------------

      void CONetCDF4::getWriteDataInfos(const StdString & name, StdSize record, StdSize & array_size,
                                        std::vector<StdSize> & sstart,
                                        std::vector<StdSize> & scount,
                                        const std::vector<StdSize> * start,
                                        const std::vector<StdSize> * count)
      {
         std::vector<StdSize> sizes = this->getDimensions(name);       
         std::vector<StdSize>::const_iterator
            it  = sizes.begin(), end = sizes.end();
         int i = 0;

         for (;it != end; it++)
         {
            StdSize  s = *it;
            if ((start != NULL) && (count != NULL))
            {
               if (s == UNLIMITED_DIM)
               {
                  sstart.push_back(record);
                  scount.push_back(1);
               }
               else
               {
                  sstart.push_back((*start)[i]);
                  scount.push_back((*count)[i]);
                  array_size *= (*count)[i];
                  i++;
               }
            }
            else
            {
               if (s == UNLIMITED_DIM)
               {
                  sstart.push_back(record);
                  scount.push_back(1);
               }
               else
               {
                  sstart.push_back(0);
                  scount.push_back(sizes[i]);
                  array_size *= sizes[i];
               }
               i++;
            }
         }
      }

      //---------------------------------------------------------------

      template <>
         void CONetCDF4::writeData_(int grpid, int varid,
                                    const std::vector<StdSize> & sstart,
                                    const std::vector<StdSize> & scount, double * data)
      {
         CheckError(nc_put_vara_double(grpid, varid, &(sstart[0]), &(scount[0]), data));
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::writeData_(int grpid, int varid,
                                    const std::vector<StdSize> & sstart,
                                    const std::vector<StdSize> & scount, int * data)
      {
          CheckError(nc_put_vara_int(grpid, varid, &(sstart[0]), &(scount[0]), data));
      }
      
      //---------------------------------------------------------------
      
      template <>
         void CONetCDF4::writeData_(int grpid, int varid,
                                    const std::vector<StdSize> & sstart,
                                    const std::vector<StdSize> & scount, float * data)
      {
          CheckError(nc_put_vara_float(grpid, varid, &(sstart[0]), &(scount[0]), data));
      }

      //---------------------------------------------------------------

      void CONetCDF4::writeData(const ARRAY(int, 2) data, const StdString & name)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);
         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;

         this->getWriteDataInfos(name, 0, array_size,  sstart, scount, NULL, NULL);
         this->writeData_(grpid, varid, sstart, scount, data->data());
      }

      //---------------------------------------------------------------
      
      bool CONetCDF4::varExist(const StdString & varname)
      {
         int varid = 0;
         int grpid = this->getCurrentGroup();
         return (nc_inq_varid (grpid, varname.c_str(), &varid) == NC_NOERR);
      }

      ///--------------------------------------------------------------
   } // namespace io
} // namespace xmlioserver
