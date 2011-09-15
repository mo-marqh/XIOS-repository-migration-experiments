
#include "nc4_data_output.hpp"

#include <boost/lexical_cast.hpp>
#include "attribute_template_impl.hpp"
#include "group_template_impl.hpp"

#include "file.hpp"
#include "calendar.hpp"

namespace xmlioserver
{
   namespace io
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CNc4DataOutput::CNc4DataOutput
         (const StdString & filename, bool exist)
            : SuperClass()
            , SuperClassWriter(filename, exist)
            , filename(filename)
      {
         StdString timeid = StdString("time_counter");
         SuperClass::type = MULTI_FILE;
         if (!exist)
            SuperClassWriter::addDimension(timeid);
      }

      CNc4DataOutput::CNc4DataOutput
         (const StdString & filename, bool exist, bool multigroup, MPI_Comm comm_server)
            : SuperClass()
            , SuperClassWriter(filename, exist, &comm_server)
            , comm_server(comm_server)
            , filename(filename)
      {
         StdString timeid = StdString("time_counter");
         SuperClass::type = (multigroup) ? MULTI_GROUP : ONE_FILE;
         if (!exist)
            SuperClassWriter::addDimension(timeid);
      }

      CNc4DataOutput::CNc4DataOutput
         (const StdString & filename, bool exist, bool multigroup, comm::MPIComm comm_server, bool)
            : SuperClass()
            , SuperClassWriter(filename, exist, &comm_server, true)
            , filename(filename)
      {
         StdString timeid = StdString("time_counter");
         SuperClass::type = (multigroup) ? MULTI_GROUP : ONE_FILE;
         if (!exist)
            SuperClassWriter::addDimension(timeid);
      }

      CNc4DataOutput::~CNc4DataOutput(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      const StdString & CNc4DataOutput::getFileName(void) const
      {
         return (this->filename);
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeDomain_(const boost::shared_ptr<tree::CDomain> domain)
      {
         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();
         
         if (domain->isEmpty()) return;

         std::vector<StdString> dim0, dim1;
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString lonid     = StdString("lon_").append(domid);
         StdString latid     = StdString("lat_").append(domid);
         StdString lonid_loc = StdString("lon_").append(domid).append("_local");
         StdString latid_loc = StdString("lat_").append(domid).append("_local");
         StdString maskid    = StdString("mask_").append(domid).append("_local");

         ARRAY(int, 2) mask = domain->getLocalMask();

         unsigned int ssize = domain->zoom_ni_loc.getValue() * domain->zoom_nj_loc.getValue();
         bool isCurvilinear = (domain->lonvalue.getValue()->size() == ssize);

         SuperClassWriter::addDimension(lonid, domain->ni_glo.getValue());
         SuperClassWriter::addDimension(latid, domain->nj_glo.getValue());

         if (isCurvilinear)
         {
            dim0.push_back(latid_loc); dim0.push_back(lonid_loc);
            lonid = StdString("nav_lon_").append(domid);
            latid = StdString("nav_lat_").append(domid);
         }
         else
         {
            dim0.push_back(latid_loc);
            dim1.push_back(lonid_loc);
         }
         switch (SuperClass::type)
         {
            case (MULTI_FILE) :
            {
               SuperClassWriter::addDimension(lonid_loc, domain->zoom_ni_loc.getValue());
               SuperClassWriter::addDimension(latid_loc, domain->zoom_nj_loc.getValue());
               this->writeLocalAttributes(domain->zoom_ibegin_loc.getValue(),
                                          domain->zoom_ni_loc.getValue(),
                                          domain->zoom_jbegin_loc.getValue(),
                                          domain->zoom_nj_loc.getValue(),
                                          domid);
               if (isCurvilinear)
               {
                  SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                  SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
               }
               else
               {
                  SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                  SuperClassWriter::addVariable(lonid, NC_FLOAT, dim1);
               }
               this->writeAxisAttributes
                  (latid, "X", "longitude", "Longitude", "degrees_east", domid);
               this->writeAxisAttributes
                  (lonid, "Y", "latitude", "Latitude", "degrees_north", domid);

               dim0.clear();
               dim0.push_back(latid_loc);
               dim0.push_back(lonid_loc);

               SuperClassWriter::addVariable(maskid, NC_INT, dim0);

               this->writeMaskAttributes(maskid,
                  domain->data_dim.getValue()/*,
                  domain->data_ni.getValue(),
                  domain->data_nj.getValue(),
                  domain->data_ibegin.getValue(),
                  domain->data_jbegin.getValue()*/);
                  
               //SuperClassWriter::setDefaultValue(maskid, &dvm);

               SuperClassWriter::definition_end();
               SuperClassWriter::writeData(domain->latvalue.getValue(), latid, true, 0);
               SuperClassWriter::writeData(domain->lonvalue.getValue(), lonid, true, 0);
               SuperClassWriter::writeData(mask, maskid);
               SuperClassWriter::definition_start();

               break;
            }
            default :
               ERROR("CNc4DataOutput::writeDomain(domain)",
                     << "[ type = " << SuperClass::type << "]"
                     << " not implemented yet !");
         }
         domain->addRelFile(this->filename);
      }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeAxis_(const boost::shared_ptr<tree::CAxis> axis)
      {
         if (axis->IsWritten(this->filename)) return;
         axis->checkAttributes();
         std::vector<StdString> dims;
         StdString axisid = (!axis->name.isEmpty())
                           ? axis->name.getValue() : axis->getId();
         SuperClassWriter::addDimension(axisid, axis->size.getValue());
         dims.push_back(axisid);

         switch (SuperClass::type)
         {
            case (MULTI_FILE) :
            {
               SuperClassWriter::addVariable(axisid, NC_FLOAT, dims);

               SuperClassWriter::addAttribute("axis", StdString("Z"), &axisid);

               if (!axis->standard_name.isEmpty())
                  SuperClassWriter::addAttribute
                     ("standard_name",  axis->standard_name.getValue(), &axisid);

               if (!axis->long_name.isEmpty())
                  SuperClassWriter::addAttribute
                     ("long_name", axis->long_name.getValue(), &axisid);

               if (!axis->unit.isEmpty())
                  SuperClassWriter::addAttribute
                     ("units", axis->unit.getValue(), &axisid);

               SuperClassWriter::definition_end();
               SuperClassWriter::writeData(axis->zvalue.getValue(), axisid, true, 0);
               SuperClassWriter::definition_start();

               break;
            }
            default :
               ERROR("CNc4DataOutput::writeDomain(domain)",
                     << "[ type = " << SuperClass::type << "]"
                     << " not implemented yet !");
         }
         axis->addRelFile(this->filename);
      }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeField_(const boost::shared_ptr<tree::CField> field)
      {
         std::vector<StdString> dims, coodinates;
         boost::shared_ptr<CGrid> grid =
            CObjectFactory::GetObject<CGrid>(field->grid_ref.getValue());
         boost::shared_ptr<CDomain> domain =
            CObjectFactory::GetObject<CDomain>(grid->domain_ref.getValue());
            
         if (domain->isEmpty()) return;

         StdString timeid    = StdString("time_counter");
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString lonid     = StdString("lon_").append(domid);
         StdString latid     = StdString("lat_").append(domid);
         StdString lonid_loc = StdString("lon_").append(domid).append("_local");
         StdString latid_loc = StdString("lat_").append(domid).append("_local");
         StdString fieldid   = (!field->name.isEmpty())
                             ? field->name.getValue() : field->getBaseFieldReference()->getId();

         unsigned int ssize = domain->zoom_ni_loc.getValue() * domain->zoom_nj_loc.getValue();
         bool isCurvilinear = (domain->lonvalue.getValue()->size() == ssize);

         nc_type type = (!field->prec.isEmpty() &&
                        ( field->prec.getValue() == 4))
                        ? NC_FLOAT : NC_DOUBLE;
         bool wtime   = !(!field->operation.isEmpty() &&
                         ( field->operation.getValue().compare("once") == 0));
                         
         if (wtime)
         {
            StdOStringStream oss;
            oss << "time_" << field->operation.getValue()
                << "_" << field->getRelFile()->output_freq.getValue();

            coodinates.push_back(oss.str());
            dims.push_back(timeid);
         }

         if (!grid->axis_ref.isEmpty())
         {
            boost::shared_ptr<CAxis> axis =
               CObjectFactory::GetObject<CAxis>(grid->axis_ref.getValue());
            StdString axisid = (!axis->name.isEmpty())
                             ? axis->name.getValue() : axis->getId();
            dims.push_back(axisid);
            coodinates.push_back(axisid);
         }

         if (isCurvilinear)
         {
            coodinates.push_back(StdString("nav_lat_").append(domid));
            coodinates.push_back(StdString("nav_lon_").append(domid));
         }
         else
         {
            coodinates.push_back(latid);
            coodinates.push_back(lonid);
         }

         switch (SuperClass::type)
         {
            case (MULTI_FILE) :
            {
               dims.push_back(latid_loc);
               dims.push_back(lonid_loc);
               SuperClassWriter::addVariable(fieldid, type, dims);

               if (!field->standard_name.isEmpty())
                  SuperClassWriter::addAttribute
                     ("standard_name",  field->standard_name.getValue(), &fieldid);

               if (!field->long_name.isEmpty())
                  SuperClassWriter::addAttribute
                     ("long_name", field->long_name.getValue(), &fieldid);

               if (!field->unit.isEmpty())
                  SuperClassWriter::addAttribute
                     ("units", field->unit.getValue(), &fieldid);
                     
               SuperClassWriter::addAttribute
                     ("online_operation", field->operation.getValue(), &fieldid);
                     
               if (wtime)
               {
                  SuperClassWriter::addAttribute
                        ("interval_operation", field->freq_op.getValue(), &fieldid);
                  SuperClassWriter::addAttribute
                        ("interval_write", field->getRelFile()->output_freq.getValue(), &fieldid);
               }
               
               if (!field->default_value.isEmpty())
               {
                  double default_value = field->default_value.getValue();
                  float fdefault_value = (float)default_value;
                  if (type == NC_DOUBLE)
                     SuperClassWriter::setDefaultValue(fieldid, &default_value);
                  else
                     SuperClassWriter::setDefaultValue(fieldid, &fdefault_value);
               }
               else
               {
                  double * default_value = NULL;
                  SuperClassWriter::setDefaultValue(fieldid, default_value);
               }             

               {  // Ecriture des coordonnées
               
                  StdString coordstr; //boost::algorithm::join(coodinates, " ")
                  std::vector<StdString>::iterator 
                     itc = coodinates.begin(), endc = coodinates.end();
                  
                  for (; itc!= endc; itc++)
                  {
                     StdString & coord = *itc;
                     if (itc+1 != endc)
                           coordstr.append(coord).append(" ");
                     else  coordstr.append(coord);
                  }

                  SuperClassWriter::addAttribute("coordinates", coordstr, &fieldid);

               }

               break;
            }
            default :
               ERROR("CNc4DataOutput::writeDomain(domain)",
                     << "[ type = " << SuperClass::type << "]"
                     << " not implemented yet !");
         }
      }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeFile_ (const boost::shared_ptr<tree::CFile> file)
      {
         StdString filename = (!file->name.isEmpty())
                            ? file->name.getValue() : file->getId();
         StdString description = (!file->description.isEmpty())
                               ? file->description.getValue()
                               : StdString("Created by xmlioserver");
         this->writeFileAttributes(filename, description,
                                   StdString ("CF-1.1"),
                                   StdString("An IPSL model"),
                                   this->getTimeStamp());
      }

      //---------------------------------------------------------------

      StdString CNc4DataOutput::getTimeStamp(void) const
      {
         const int buffer_size = 100;
         time_t rawtime;
         struct tm * timeinfo = NULL;
         char buffer [buffer_size];

         time ( &rawtime );
         timeinfo = localtime ( &rawtime );
         strftime (buffer, buffer_size, "%Y-%b-%d %H:%M:%S %Z", timeinfo);

         return (StdString(buffer));
      }
      
      //---------------------------------------------------------------
      
      void CNc4DataOutput::writeFieldData_ (const boost::shared_ptr<tree::CField>  field)
      {
         boost::shared_ptr<CGrid> grid =
            CObjectFactory::GetObject<CGrid>(field->grid_ref.getValue());
         StdString fieldid   = (!field->name.isEmpty())
                             ? field->name.getValue() 
                             : field->getBaseFieldReference()->getId();
         ARRAY(double, 1) field_data = field->getData();
         
         if (grid->hasAxis()) // 3D
         {
            ARRAY(double, 3) field_data3D (new CArray<double,3>(grid->getLocalShape()/*, boost::c_storage_order()*/));            
            grid->outputField(field_data, field_data3D);
            SuperClassWriter::writeData(field_data3D, fieldid, true, field->getNStep()-1);
            
         }
         else // 2D
         {
            ARRAY(double, 2) field_data2D (new CArray<double, 2>(grid->getLocalShape()/*, boost::c_storage_order()*/));
            grid->outputField(field_data,  field_data2D);
            SuperClassWriter::writeData(field_data2D, fieldid, true, field->getNStep()-1);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxis_
                  (const boost::shared_ptr<tree::CField>    field,
                   const boost::shared_ptr<date::CCalendar> cal)
      {
         StdOStringStream oss;
         oss << "time_" << field->operation.getValue()
             << "_" << field->getRelFile()->output_freq.getValue();

         std::vector<StdString> dims;
         StdString axisid = oss.str();
         StdString timeid = StdString("time_counter");

         dims.push_back(timeid);
         if (!SuperClassWriter::varExist(axisid))
         {
            SuperClassWriter::addVariable(axisid, NC_DOUBLE, dims);
            this->writeTimeAxisAttributes
               (axisid, cal->getId(),
                StdString("seconds since ").append(cal->getInitDate().toString()),
                cal->getInitDate().toString());
         }

      }

      //---------------------------------------------------------------
      
      void CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name,
                                                   const StdString & calendar,
                                                   const StdString & units,
                                                   const StdString & time_origin,
                                                   const StdString & standard_name,
                                                   const StdString & long_name,
                                                   const StdString & title)
      {
         SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
         SuperClassWriter::addAttribute("long_name",     long_name    , &axis_name);
         SuperClassWriter::addAttribute("title",         title        , &axis_name);
         SuperClassWriter::addAttribute("calendar",      calendar     , &axis_name);
         SuperClassWriter::addAttribute("units",         units        , &axis_name);
         SuperClassWriter::addAttribute("time_origin",   time_origin  , &axis_name);
      }
      
      //---------------------------------------------------------------

      void CNc4DataOutput::writeAxisAttributes(const StdString & axis_name,
                                               const StdString & axis,
                                               const StdString & standard_name,
                                               const StdString & long_name,
                                               const StdString & units,
                                               const StdString & nav_model)
      {
         SuperClassWriter::addAttribute("axis"         , axis         , &axis_name);
         SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
         SuperClassWriter::addAttribute("long_name"    , long_name    , &axis_name);
         SuperClassWriter::addAttribute("units"        , units        , &axis_name);
         SuperClassWriter::addAttribute("nav_model"    , nav_model    , &axis_name);
      }

      //---------------------------------------------------------------
      
      void CNc4DataOutput::writeLocalAttributes
         (int ibegin, int ni, int jbegin, int nj, StdString domid)
      {
         SuperClassWriter::addAttribute(StdString("ibegin_").append(domid), ibegin);
         SuperClassWriter::addAttribute(StdString("ni_"    ).append(domid), ni);
         SuperClassWriter::addAttribute(StdString("jbegin_").append(domid), jbegin);
         SuperClassWriter::addAttribute(StdString("nj_"    ).append(domid), nj);
      }

      //---------------------------------------------------------------

      void CNc4DataOutput:: writeFileAttributes(const StdString & name,
                                                const StdString & description,
                                                const StdString & conventions,
                                                const StdString & production,
                                                const StdString & timeStamp)
      {
         SuperClassWriter::addAttribute("name"       , name);
         SuperClassWriter::addAttribute("description", description);
         SuperClassWriter::addAttribute("conventions", conventions);
         SuperClassWriter::addAttribute("production" , production);
         SuperClassWriter::addAttribute("timeStamp"  , timeStamp);
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeMaskAttributes(const StdString & mask_name,
                                               int data_dim,
                                               int data_ni,
                                               int data_nj,
                                               int data_ibegin,
                                               int data_jbegin)
      {
         SuperClassWriter::addAttribute("data_dim"   , data_dim   , &mask_name);
         SuperClassWriter::addAttribute("data_ni"    , data_ni    , &mask_name);
         SuperClassWriter::addAttribute("data_nj"    , data_nj    , &mask_name);
         SuperClassWriter::addAttribute("data_ibegin", data_ibegin, &mask_name);
         SuperClassWriter::addAttribute("data_jbegin", data_jbegin, &mask_name);
      }

      ///--------------------------------------------------------------

   } // namespace io
} // namespace xmlioserver
