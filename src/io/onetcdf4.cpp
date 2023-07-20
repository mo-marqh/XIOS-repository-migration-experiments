#include <fstream>

#include "onetcdf4.hpp"
#include "onetcdf4_plugin.hpp"
#include "group_template.hpp"
#include "mpi.hpp"
#include "netcdf.hpp"
#include "netCdfInterface.hpp"
#include "netCdfException.hpp"
#include "timer.hpp"

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///

      CONetCDF4::CONetCDF4(const StdString& filename, bool append, bool useClassicFormat,
							bool useCFConvention,
                           const MPI_Comm* comm, bool multifile, const StdString& timeCounterName)
        : path()
        , wmpi(false)
        , useClassicFormat(useClassicFormat)
        , useCFConvention(useCFConvention)
      {
         this->initialize(filename, append, useClassicFormat, useCFConvention, comm, multifile, timeCounterName);
      }

      //---------------------------------------------------------------

      CONetCDF4::~CONetCDF4(void)
      {
      }

      ///--------------------------------------------------------------

      void CONetCDF4::initialize(const StdString& filename, bool append, bool useClassicFormat, bool useCFConvention, 
                                 const MPI_Comm* comm, bool multifile, const StdString& timeCounterName)
      {
         this->useClassicFormat = useClassicFormat;
         this->useCFConvention = useCFConvention;

         int mode = useClassicFormat ? 0 : NC_NETCDF4;

         // Don't use parallel mode if there is only one process
         if (comm)
         {
            int commSize = 0;
            MPI_Comm_size(*comm, &commSize);
            if (commSize <= 1)
               comm = NULL;
         }
         wmpi = comm && !multifile;

         if (wmpi)
            mode |= useClassicFormat ? NC_PNETCDF : NC_MPIIO;

         // If the file does not exist, we always create it
         if (!append || !std::ifstream(filename.c_str()))
         {
            CTimer::get("Files : create").resume();
            if (wmpi)
               CNetCdfInterface::createPar(filename, mode, *comm, MPI_INFO_NULL, this->ncidp);
            else
               CNetCdfInterface::create(filename, mode, this->ncidp);
            CTimer::get("Files : create").suspend();
 
            this->appendMode = false;
         }
         else
         {
            mode |= NC_WRITE;
            CTimer::get("Files : open").resume();
            if (wmpi)
               CNetCdfInterface::openPar(filename, mode, *comm, MPI_INFO_NULL, this->ncidp);
            else
               CNetCdfInterface::open(filename, mode, this->ncidp);
            CTimer::get("Files : open").suspend();
            this->appendMode = true;
         }

         // If the classic NetCDF format is used, we enable the "no-fill mode" globally.
         // This is done per variable for the NetCDF4 format.
         if (useClassicFormat)
            CNetCdfInterface::setFill(this->ncidp, false);

         this->timeCounterName = timeCounterName;
      }

      void CONetCDF4::close()
      {
        CTimer::get("Files : close").resume();
        CNetCdfInterface::close(this->ncidp);
        CTimer::get("Files : close").suspend();
      }

      //---------------------------------------------------------------

      void CONetCDF4::definition_start(void)
      {
         CNetCdfInterface::reDef(this->ncidp);
      }

      //---------------------------------------------------------------

      void CONetCDF4::definition_end(void)
      {
         CNetCdfInterface::endDef(this->ncidp);
      }

      //---------------------------------------------------------------

      int CONetCDF4::getCurrentGroup(void)
      {
         return this->getGroup(this->getCurrentPath());
      }

      //---------------------------------------------------------------

      int CONetCDF4::getGroup(const CONetCDF4Path& path)
      {
         int retvalue = this->ncidp;

         CONetCDF4Path::const_iterator it = path.begin(), end = path.end();

         for (; it != end; it++)
         {
            const StdString& groupid = *it;
            CNetCdfInterface::inqNcId(retvalue, groupid, retvalue);
         }
         return retvalue;
      }

      //---------------------------------------------------------------

      int CONetCDF4::getVariable(const StdString& varname)
      {
         int varid = 0;
         int grpid = this->getCurrentGroup();
         CNetCdfInterface::inqVarId(grpid, varname, varid);
         return varid;
      }

      //---------------------------------------------------------------

      int CONetCDF4::getDimension(const StdString& dimname)
      {
         int dimid = 0;
         int grpid = this->getCurrentGroup();
         CNetCdfInterface::inqDimId(grpid, dimname, dimid);
         return dimid;
      }

      //---------------------------------------------------------------

      int CONetCDF4::getUnlimitedDimension(void)
      {
         int dimid = 0;
         int grpid = this->getCurrentGroup();
         CNetCdfInterface::inqUnLimDim(grpid, dimid);
         return dimid;
      }

      StdString CONetCDF4::getUnlimitedDimensionName(void)
      {
         int grpid = this->getGroup(path);
         int dimid = this->getUnlimitedDimension();

         StdString dimname;
         if (dimid != -1)
           CNetCdfInterface::inqDimName(grpid, dimid, dimname);
         return dimname;
      }

      //---------------------------------------------------------------

      std::vector<StdSize> CONetCDF4::getDimensions(const StdString& varname)
      {
         StdSize size = 0;
         std::vector<StdSize> retvalue;
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(varname);
         int nbdim = 0, *dimid = NULL;

         CNetCdfInterface::inqVarNDims(grpid, varid, nbdim);
         dimid = new int[nbdim]();
         CNetCdfInterface::inqVarDimId(grpid, varid, dimid);

         for (int i = 0; i < nbdim; i++)
         {
            CNetCdfInterface::inqDimLen(grpid, dimid[i], size);
            if (size == NC_UNLIMITED)
                size = UNLIMITED_DIM;
            retvalue.push_back(size);
         }
         delete [] dimid;
         return retvalue;
      }

      std::vector<std::string> CONetCDF4::getDimensionsIdList(const std::string* _varname)
      {
         int nDimNull = 0;
         int nbdim = 0, *dimid = NULL;
         int grpid = this->getCurrentGroup();
         int varid = (_varname != NULL) ? this->getVariable(*_varname) : NC_GLOBAL;
         std::vector<std::string> retvalue;

         if (_varname != NULL)
         {
            CNetCdfInterface::inqVarNDims(grpid, varid, nbdim);
            dimid = new int[nbdim]();
            CNetCdfInterface::inqVarDimId(grpid, varid, dimid);
         }
         else
         {
            CNetCdfInterface::inqDimIds(grpid, nbdim, NULL, 1);
            dimid = new int[nbdim]();
            CNetCdfInterface::inqDimIds(grpid, nDimNull, dimid, 1);
         }

         for (int i = 0; i < nbdim; i++)
         {
            std::string dimname;
            CNetCdfInterface::inqDimName(grpid, dimid[i], dimname);
            retvalue.push_back(dimname);
         }
         delete [] dimid;

         return retvalue;
      }

      //---------------------------------------------------------------

      void CONetCDF4::getTimeAxisBounds(CArray<double,2>& timeAxisBounds, const StdString& name, bool collective)
      {
        int grpid = this->getCurrentGroup();
        int varid = this->getVariable(name);

        std::vector<StdSize> start(2), count(2);
        start[0] = 0;
        // Find out how many temporal records have been written already to the file we are opening
        int ncUnlimitedDimId;
        CNetCdfInterface::inqUnLimDim(this->ncidp, ncUnlimitedDimId);
        CNetCdfInterface::inqDimLen(this->ncidp, ncUnlimitedDimId, count[0]);
        start[1] = 0;
        count[1] = 2;

        timeAxisBounds.resize(count[1], count[0]);

        if (this->wmpi && collective)
          CNetCdfInterface::varParAccess(grpid, varid, NC_COLLECTIVE);
        if (this->wmpi && !collective)
          CNetCdfInterface::varParAccess(grpid, varid, NC_INDEPENDENT);

        CNetCdfInterface::getVaraType(grpid, varid, &start[0], &count[0], timeAxisBounds.dataFirst());
      }

      void CONetCDF4::getTimeAxisBounds(CArray<double,2>& timeAxisBounds, const StdString& name, bool collective, size_t record)
      {
        int grpid = this->getCurrentGroup();
        int varid = this->getVariable(name);

        std::vector<StdSize> start(2), count(2);
        start[0] = record;
        count[0] = 1 ;
        start[1] = 0;
        count[1] = 2;

        timeAxisBounds.resize(2, 1);

        if (this->wmpi && collective)
          CNetCdfInterface::varParAccess(grpid, varid, NC_COLLECTIVE);
        if (this->wmpi && !collective)
          CNetCdfInterface::varParAccess(grpid, varid, NC_INDEPENDENT);

        CNetCdfInterface::getVaraType(grpid, varid, &start[0], &count[0], timeAxisBounds.dataFirst());
      }



      const CONetCDF4::CONetCDF4Path& CONetCDF4::getCurrentPath(void) const
      { return this->path; }

      void CONetCDF4::setCurrentPath(const CONetCDF4Path& path)
      { this->path = path; }

      //---------------------------------------------------------------

      int CONetCDF4::addGroup(const StdString& name)
      {
         int retvalue = 0;
         int grpid = this->getCurrentGroup();
         CNetCdfInterface::defGrp(grpid, name, retvalue);
         return retvalue;
      }

      //---------------------------------------------------------------

      int CONetCDF4::addDimension(const StdString& name, const StdSize size)
      {
         int retvalue = 0;
         int grpid = this->getCurrentGroup();
         if (size != UNLIMITED_DIM)
            CNetCdfInterface::defDim(grpid, name, size, retvalue);
         else
            CNetCdfInterface::defDim(grpid, name, NC_UNLIMITED, retvalue);
         return retvalue;
      }

      //---------------------------------------------------------------

      int CONetCDF4::addVariable(const StdString& name, nc_type type,
                                 const std::vector<StdString>& dim, bool defineChunking)
      {
         int varid = 0;
         std::vector<int> dimids;
         std::vector<StdSize> dimsizes;
         int dimSize = dim.size();

         StdSize size;

         int grpid = this->getCurrentGroup();

         std::vector<StdString>::const_iterator it = dim.begin(), end = dim.end();

         for (int idx = 0; it != end; it++, ++idx)
         {
            const StdString& dimid = *it;
            dimids.push_back(this->getDimension(dimid));
            CNetCdfInterface::inqDimLen(grpid, this->getDimension(dimid), size);
            if (size == NC_UNLIMITED) size = 1;
            dimsizes.push_back(size);
         }

         CNetCdfInterface::defVar(grpid, name, type, dimids.size(), &dimids[0], varid);

         if (defineChunking) {
           int storageType = (0 == dimSize) ? NC_CONTIGUOUS : NC_CHUNKED;
           CNetCdfInterface::defVarChunking(grpid, varid, storageType, &dimsizes[0]);
           CNetCdfInterface::defVarFill(grpid, varid, true, NULL);
         }

         
         return varid;
      }

      //---------------------------------------------------------------

      int CONetCDF4::addChunk(CField* field, nc_type type,
                              const std::vector<StdString>& dim, int compressionLevel)
      {
         const StdString& name = field->getFieldOutputName();
         int varid = 0;
         std::vector<StdSize> dimsizes;
         int dimSize = dim.size();
         
         StdSize size;
         StdSize totalSize;

         // default chunk size         
         StdSize maxSize = 1024 * 1024 * 20; // = 20 Mo (exp using large NEMO like grids)
         StdSize targetSize = maxSize;
         if (!field->chunking_blocksize_target.isEmpty())
         {
           targetSize = field->chunking_blocksize_target.getValue()*1024*1024;
         }
         StdSize recordSize = 1; //sizeof(type);
         if (field->prec.isEmpty()) recordSize *= 4;
         else recordSize *= field->prec;

         int grpid = this->getCurrentGroup();

         std::vector<StdString>::const_iterator it = dim.begin(), end = dim.end();

         for (int idx = 0; it != end; it++, ++idx)
         {
            const StdString& dimid = *it;
            CNetCdfInterface::inqDimLen(grpid, this->getDimension(dimid), size);
            if (size == NC_UNLIMITED) size = 1;
            recordSize *= size;
            dimsizes.push_back(size);
         }
         double chunkingRatio = (double)recordSize / (double)targetSize;

         varid = this->getVariable(name);

         // The classic NetCDF format does not support chunking nor fill parameters
         if (!useClassicFormat)
         {
            // Browse field's elements (domain, axis) and NetCDF meta-data to retrieve corresponding chunking coefficients
            //   in the file order !
            CGrid* grid = field->getGrid();
            std::vector<CDomain*> domains = grid->getDomains();
            std::vector<CAxis*> axis = grid->getAxis();

            std::vector<double> userChunkingWeights; // store chunking coefficients defined by users
	    std::vector<StdString>::const_reverse_iterator itId = dim.rbegin(); // NetCDF is fed using dim, see this::addVariable()
            int elementIdx(0);
            int outerAxis(-1), outerDomJ(-1), outerDomI(-1);
            for (vector<StdSize>::reverse_iterator itDim = dimsizes.rbegin(); itDim != dimsizes.rend(); ++itDim, ++itId, ++elementIdx)
            {
              bool isAxis(false);
              for ( auto ax : axis )
              {
                StdString axisDim;
                // Rebuild axis name from axis before comparing
                if (ax->dim_name.isEmpty()) axisDim = ax->getAxisOutputName();
                else axisDim=ax->dim_name.getValue();
                if (axisDim == *itId)
                {
                  if (!ax->chunking_weight.isEmpty()) userChunkingWeights.push_back( ax->chunking_weight.getValue() );
                  else userChunkingWeights.push_back( 0. );
                  isAxis = true;
                  outerAxis = elementIdx; // Going backward in dimsizes, overwriting will keep outer
                  break;
                }
              } // end scanning axis
              bool isDomain(false);
              for ( auto dom : domains )
              {
                StdString axisDim;
                // Rebuild axis I name from domain before comparing
                if (!dom->dim_i_name.isEmpty()) axisDim = dom->dim_i_name.getValue();
                else
                {
                  if (dom->type==CDomain::type_attr::curvilinear)  axisDim="x";
                  if (dom->type==CDomain::type_attr::unstructured) axisDim="cell";
                  if (dom->type==CDomain::type_attr::rectilinear)  axisDim="lon";
                }
                if (axisDim == *itId)
                {
                  if (!dom->chunking_weight_i.isEmpty()) userChunkingWeights.push_back( dom->chunking_weight_i.getValue() );
                  else userChunkingWeights.push_back( 0. );
                  isDomain = true;
                  outerDomI = elementIdx; // Going backward in dimsizes, overwriting will keep outer
                  break;
                }
                // Rebuild axis J name from domain before comparing
                if (!dom->dim_j_name.isEmpty()) axisDim = dom->dim_j_name.getValue();
                else {
                  if (dom->type==CDomain::type_attr::curvilinear)  axisDim="y";
                  if (dom->type==CDomain::type_attr::rectilinear)  axisDim="lat";
                }
                if (axisDim == *itId)
                {
                  if (!dom->chunking_weight_j.isEmpty()) userChunkingWeights.push_back( dom->chunking_weight_j.getValue() );
                  else userChunkingWeights.push_back( 0. );
                  outerDomJ = elementIdx; // Going backward in dimsizes, overwriting will keep outer
                  isDomain = true;
                  break;
                }
              } // end scanning domain
              // No chunking applied on scalars or time
              if ((!isAxis)&&(!isDomain))
              {
                userChunkingWeights.push_back( 0. );
              }
            }

            double sumChunkingWeights(0);
            for (int i=0;i<userChunkingWeights.size();i++) sumChunkingWeights += userChunkingWeights[i];
            if ( (!sumChunkingWeights) && (chunkingRatio > 1) )
            {
              if (outerAxis!=-1) userChunkingWeights[outerAxis] = 1;      // chunk along outer axis
              else if (outerDomJ!=-1) userChunkingWeights[outerDomJ] = 1; // if no axis ? -> along j
              else if (outerDomI!=-1) userChunkingWeights[outerDomI] = 1; // if no j      -> along i
              else {;}
            }

            int countChunkingDims(0); // number of dimensions on which chunking is operated : algo uses pow(value, 1/countChunkingDims)
            double normalizingWeight(0); // use to relativize chunking coefficients for all dimensions
            for (int i=0;i<userChunkingWeights.size();i++)
              if (userChunkingWeights[i]>0.)
              {
                countChunkingDims++;
                normalizingWeight = userChunkingWeights[i];
              }

            std::vector<double> chunkingRatioPerDims; // will store coefficients used to compute chunk size
            double productRatios = 1; // last_coeff = pow( shrink_ratio / (product of all ratios), 1/countChunkingDims )
            for (int i=0;i<userChunkingWeights.size();i++)
            {
              chunkingRatioPerDims.push_back( userChunkingWeights[i] / normalizingWeight );
              if (chunkingRatioPerDims[i]) productRatios *= chunkingRatioPerDims[i];
            }
            for (int i=0;i<userChunkingWeights.size();i++)
            {
              chunkingRatioPerDims[i] *= pow( chunkingRatio / productRatios, 1./countChunkingDims );
            }
            
            std::vector<double>::iterator itChunkingRatios = chunkingRatioPerDims.begin();
            //itId = dim.rbegin();
            double correctionFromPreviousDim = 1.;
            for (vector<StdSize>::reverse_iterator itDim = dimsizes.rbegin(); itDim != dimsizes.rend(); ++itDim, ++itChunkingRatios, ++itId)
            {
              *itChunkingRatios *= correctionFromPreviousDim;
              correctionFromPreviousDim = 1;
              if (*itChunkingRatios > 1) // else target larger than size !
              {
                StdSize dimensionSize = *itDim;
                //info(0) << *itId << " " << *itDim << " " << *itChunkingRatios << " " << (*itDim)/(*itChunkingRatios) << endl;
                *itDim = ceil( *itDim / ceil(*itChunkingRatios) );
                correctionFromPreviousDim = *itChunkingRatios/ ((double)dimensionSize/(*itDim));
              }
            }
            int storageType = (0 == dimSize) ? NC_CONTIGUOUS : NC_CHUNKED;
            CNetCdfInterface::defVarChunking(grpid, varid, storageType, &dimsizes[0]);
            CNetCdfInterface::defVarFill(grpid, varid, true, NULL);
         }
         
         return varid;
      }

      //---------------------------------------------------------------

      void CONetCDF4::setCompressionLevel(const StdString& varname, const StdString& compressionType, int compressionLevel, const CArray<double,1>& compressionParams)
      {
         if (compressionLevel < 0 || compressionLevel > 9)
           ERROR("void CONetCDF4::setCompressionLevel(const StdString& varname, int compressionLevel)",
                 "Invalid compression level for variable \"" << varname << "\", the value should range between 0 and 9.");
#ifndef PARALLEL_COMPRESSION
         if ( ((compressionLevel)||(compressionParams.numElements())) && wmpi)
           ERROR("void CONetCDF4::setCompressionLevel(const StdString& varname, int compressionLevel)",
                 "Impossible to use compression for variable \"" << varname << "\" when using parallel mode.");
#endif
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(varname);
         if (compressionType=="None")
         {
         }
         else if (compressionType=="gzip")
         {
           CNetCdfInterface::defVarDeflate(grpid, varid, compressionLevel);
         }
         else
         {
           size_t cd_nelmts;
           unsigned int* cd_values = NULL;
           if (compressionType=="SZ")
           {
             CONetCDF4Plugin::interpretParametersSZ(compressionParams, &cd_nelmts, &cd_values);
             CNetCdfInterface::defVarFilter(grpid, varid, 32017, cd_nelmts, cd_values);
           }
           else if (compressionType=="ZFP")
           {
             CONetCDF4Plugin::interpretParametersZFP(compressionParams, &cd_nelmts, &cd_values);
             CNetCdfInterface::defVarFilter(grpid, varid, 32013, cd_nelmts, cd_values);
           }
           else
           {
               ERROR("void CONetCDF4::setCompressionLevel(...)", "compression_type = " << compressionType << " is not managed");
           }
           if (cd_values!=NULL) delete [] cd_values;
         }
                      
      }

      //---------------------------------------------------------------

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const StdString& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, value.size(), value.c_str());
      }

      //---------------------------------------------------------------

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const double& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, 1, &value);
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const CArray<double,1>& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, value.numElements(), value.dataFirst());
      }
      //---------------------------------------------------------------

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const float& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, 1, &value);
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const CArray<float,1>& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, value.numElements(), value.dataFirst());
      }

      //---------------------------------------------------------------

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const int& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, 1, &value);
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const CArray<int,1>& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, value.numElements(), value.dataFirst());
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const short int& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, 1, &value);
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const CArray<short int,1>& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, value.numElements(), value.dataFirst());
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const long int& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, 1, &value);
      }

      template <>
      void CONetCDF4::addAttribute(const StdString& name, const CArray<long int,1>& value, const StdString* varname)
      {
         int grpid = this->getCurrentGroup();
         int varid = (varname == NULL) ? NC_GLOBAL : this->getVariable(*varname);
         CNetCdfInterface::putAttType(grpid, varid, name, value.numElements(), value.dataFirst());
      }

      //---------------------------------------------------------------

      void CONetCDF4::getWriteDataInfos(const StdString& name, StdSize record, StdSize& array_size,
                                        std::vector<StdSize>& sstart,
                                        std::vector<StdSize>& scount,
                                        const std::vector<StdSize>* start,
                                        const std::vector<StdSize>* count)
      {
         std::vector<std::size_t> sizes  = this->getDimensions(name);
         if (sizes.size()==0) 
         {
            
            if ((start != NULL) && (count != NULL) && start->size()==1 && count->size()==1) // pur scalar case
              array_size*=(*count)[0] ;
            else array_size=1 ;
         }
         else
         {
           std::vector<std::string> iddims = this->getDimensionsIdList (&name);
           std::vector<std::size_t>::const_iterator
           it  = sizes.begin(), end = sizes.end();
           int i = 0;

           if (iddims.begin()->compare(timeCounterName) == 0)
           {
             sstart.push_back(record);
             scount.push_back(1);
             if ((start == NULL) && (count == NULL)) i++;
             it++;
             if (it==end)
             {
               if ((start != NULL) && (count != NULL) && start->size()==1 && count->size()==1) // pur scalar case
               {
                 scount[0]=(*count)[0] ;
                 array_size *= (*count)[0];
               }
             }
           }
           
           for (;it != end; it++)
           {
              if ((start != NULL) && (count != NULL))
              {
                 sstart.push_back((*start)[i]);
                 scount.push_back((*count)[i]);
                 array_size *= (*count)[i];
                 i++;
              }
              else
              {
                 sstart.push_back(0);
                 scount.push_back(sizes[i]);
                 array_size *= sizes[i];
                 i++;
              }
           }

         }
      }


      template <>
      void CONetCDF4::writeData_(int grpid, int varid,
                                 const std::vector<StdSize>& sstart,
                                 const std::vector<StdSize>& scount, const double* data)
      {
         CNetCdfInterface::putVaraType(grpid, varid, &sstart[0], &scount[0], data);
      }

      //---------------------------------------------------------------

      template <>
      void CONetCDF4::writeData_(int grpid, int varid,
                                 const std::vector<StdSize>& sstart,
                                 const std::vector<StdSize>& scount, char* data)
      {
          CNetCdfInterface::putVaraType(grpid, varid, &sstart[0], &scount[0], data);
      }
      
      template <>
      void CONetCDF4::writeData_(int grpid, int varid,
                                 const std::vector<StdSize>& sstart,
                                 const std::vector<StdSize>& scount, const int* data)
      {
          CNetCdfInterface::putVaraType(grpid, varid, &sstart[0], &scount[0], data);
      }
      //---------------------------------------------------------------

      template <>
      void CONetCDF4::writeData_(int grpid, int varid,
                                 const std::vector<StdSize>& sstart,
                                 const std::vector<StdSize>& scount, const size_t* data)
      {
          CNetCdfInterface::putVaraType(grpid, varid, &sstart[0], &scount[0], data);
      }
      //---------------------------------------------------------------

      template <>
      void CONetCDF4::writeData_(int grpid, int varid,
                                 const std::vector<StdSize>& sstart,
                                 const std::vector<StdSize>& scount, const float* data)
      {
          CNetCdfInterface::putVaraType(grpid, varid, &sstart[0], &scount[0], data);
      }

      //---------------------------------------------------------------

      void CONetCDF4::writeData(const CArray<int, 2>& data, const StdString& name)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);
         
         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;

         this->getWriteDataInfos(name, 0, array_size,  sstart, scount, NULL, NULL);

         this->writeData_(grpid, varid, sstart, scount, data.dataFirst());

      }

      void CONetCDF4::writeTimeAxisData(const CArray<double, 1>& data, const StdString& name,
                                        bool collective, StdSize record, bool isRoot)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);

         map<int,size_t>::iterator it=timeAxis.find(varid);
         if (it == timeAxis.end()) timeAxis[varid] = record;
         else
         {
           if (it->second >= record) return;
           else it->second =record;
         }

         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;

         if (this->wmpi && collective)
            CNetCdfInterface::varParAccess(grpid, varid, NC_COLLECTIVE);
         if (this->wmpi && !collective)
            CNetCdfInterface::varParAccess(grpid, varid, NC_INDEPENDENT);

         this->getWriteDataInfos(name, record, array_size,  sstart, scount, NULL, NULL);
         this->writeData_(grpid, varid, sstart, scount, data.dataFirst());
       }

      void CONetCDF4::writeTimeAxisDataBounds(const CArray<double, 1>& data, const StdString& name,
                                        bool collective, StdSize record, bool isRoot)
      {
         int grpid = this->getCurrentGroup();
         int varid = this->getVariable(name);

         map<int,size_t>::iterator it=timeAxis.find(varid);
         if (it == timeAxis.end()) timeAxis[varid] = record;
         else
         {
           if (it->second >= record) return;
           else it->second =record;
         }

         StdSize array_size = 1;
         std::vector<StdSize> sstart, scount;

         if (this->wmpi && collective)
            CNetCdfInterface::varParAccess(grpid, varid, NC_COLLECTIVE);
         if (this->wmpi && !collective)
            CNetCdfInterface::varParAccess(grpid, varid, NC_INDEPENDENT);

         this->getWriteDataInfos(name, record, array_size,  sstart, scount, NULL, NULL);
         this->writeData_(grpid, varid, sstart, scount, data.dataFirst());
       }


      //---------------------------------------------------------------

      bool CONetCDF4::varExist(const StdString& varname)
      {
         int grpid = this->getCurrentGroup();
         return CNetCdfInterface::isVarExisted(grpid, varname);
      }

      bool CONetCDF4::dimExist(const StdString& dimname)
      {
         int grpid = this->getCurrentGroup();
         return CNetCdfInterface::isDimExisted(grpid, dimname);
      }

      void CONetCDF4::sync(void)
      {
         CNetCdfInterface::sync(this->ncidp);
      }
      ///--------------------------------------------------------------
 } // namespace xios
