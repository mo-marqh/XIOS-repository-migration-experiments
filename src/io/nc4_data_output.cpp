
#include "nc4_data_output.hpp"

#include <boost/lexical_cast.hpp>
#include "attribute_template.hpp"
#include "group_template.hpp"

#include "file.hpp"
#include "calendar.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "netCdfException.hpp"
#include "exception.hpp"

namespace xios
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
      }

      CNc4DataOutput::CNc4DataOutput
         (const StdString & filename, bool exist, bool useClassicFormat,
          MPI_Comm comm_file,bool multifile, bool isCollective)
            : SuperClass()
            , SuperClassWriter(filename, exist, useClassicFormat, &comm_file, multifile)
            , comm_file(comm_file)
            , filename(filename)
            , isCollective(isCollective)
      {
         StdString timeid = StdString("time_counter");

         SuperClass::type = (multifile) ? MULTI_FILE : ONE_FILE;
      }


      CNc4DataOutput::~CNc4DataOutput(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      const StdString & CNc4DataOutput::getFileName(void) const
      {
         return (this->filename);
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeDomain_(CDomain* domain)
      {
         if (domain->type == CDomain::type_attr::unstructured)
         {
           writeUnstructuredDomain(domain) ;
           return ;
         }

         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         std::vector<StdString> dim0, dim1;
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;


         StdString dimXid, dimYid ;

         bool isRegularDomain = (domain->type == CDomain::type_attr::rectilinear);
         switch (domain->type)
         {
           case CDomain::type_attr::curvilinear :
             dimXid     = StdString("x").append(appendDomid);
             dimYid     = StdString("y").append(appendDomid);
             break ;
           case CDomain::type_attr::rectilinear :
             dimXid     = StdString("lon").append(appendDomid);
             dimYid     = StdString("lat").append(appendDomid);
             break;
         }

         StdString dimVertId = StdString("nvertex").append(appendDomid);

         string lonid,latid,bounds_lonid,bounds_latid ;
         string areaId = "area" + appendDomid;
/*
         StdString lonid_loc = (server->intraCommSize > 1)
                             ? StdString("lon").append(appendDomid).append("_local")
                             : lonid;
         StdString latid_loc = (server->intraCommSize > 1)
                             ? StdString("lat").append(appendDomid).append("_local")
                             : latid;
*/

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
  //               if (domain->isEmpty()) return;

                 if (server->intraCommSize > 1)
                 {
  //                 SuperClassWriter::addDimension(lonid, domain->zoom_ni.getValue());
  //                 SuperClassWriter::addDimension(latid, domain->zoom_nj.getValue());
                 }

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     dim0.push_back(dimYid); dim0.push_back(dimXid);
                     lonid = StdString("nav_lon").append(appendDomid);
                     latid = StdString("nav_lat").append(appendDomid);
                     break ;
                   case CDomain::type_attr::rectilinear :
                     lonid = StdString("lon").append(appendDomid);
                     latid = StdString("lat").append(appendDomid);
                     dim0.push_back(dimYid);
                     dim1.push_back(dimXid);
                     break;
                 }

                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);

                 SuperClassWriter::addDimension(dimXid, domain->zoom_ni_srv);
                 SuperClassWriter::addDimension(dimYid, domain->zoom_nj_srv);

                 if (domain->hasBounds)
                   SuperClassWriter::addDimension(dimVertId, domain->nvertex);

                 if (server->intraCommSize > 1)
                 {
                   this->writeLocalAttributes(domain->zoom_ibegin_srv,
                                              domain->zoom_ni_srv,
                                              domain->zoom_jbegin_srv,
                                              domain->zoom_nj_srv,
                                              appendDomid);

                   if (singleDomain)
                    this->writeLocalAttributes_IOIPSL(dimXid, dimYid,
                                                      domain->zoom_ibegin_srv,
                                                      domain->zoom_ni_srv,
                                                      domain->zoom_jbegin_srv,
                                                      domain->zoom_nj_srv,
                                                      domain->ni_glo,domain->nj_glo,
                                                      server->intraCommRank,server->intraCommSize);
                 }

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                       SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                       break ;
                      case CDomain::type_attr::rectilinear :
                        SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                        SuperClassWriter::addVariable(lonid, NC_FLOAT, dim1);
                        break ;
                   }

                   this->writeAxisAttributes(lonid, isRegularDomain ? "X" : "", "longitude", "Longitude", "degrees_east", domid);
                   this->writeAxisAttributes(latid, isRegularDomain ? "Y" : "", "latitude", "Latitude", "degrees_north", domid);

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::addAttribute("bounds", bounds_lonid, &lonid);
                     SuperClassWriter::addAttribute("bounds", bounds_latid, &latid);

                     dim0.clear();
                     dim0.push_back(dimYid);
                     dim0.push_back(dimXid);
                     dim0.push_back(dimVertId);
                     SuperClassWriter::addVariable(bounds_lonid, NC_FLOAT, dim0);
                     SuperClassWriter::addVariable(bounds_latid, NC_FLOAT, dim0);
                   }
                 }

                 dim0.clear();
                 dim0.push_back(dimYid);
                 dim0.push_back(dimXid);


  // supress mask               if (server->intraCommSize > 1)
  // supress mask               {
  // supress mask                  SuperClassWriter::addVariable(maskid, NC_INT, dim0);
  // supress mask
  // supress mask                  this->writeMaskAttributes(maskid,
  // supress mask                     domain->data_dim.getValue()/*,
  // supress mask                     domain->data_ni.getValue(),
  // supress mask                     domain->data_nj.getValue(),
  // supress mask                     domain->data_ibegin.getValue(),
  // supress mask                     domain->data_jbegin.getValue()*/);
  // supress mask               }

                 //SuperClassWriter::setDefaultValue(maskid, &dvm);

                 if (domain->hasArea)
                 {
                   SuperClassWriter::addVariable(areaId, NC_FLOAT, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0);
                       SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0);
                       break;
                     case CDomain::type_attr::rectilinear :
                       CArray<double,1> lat = domain->latvalue_srv(Range(fromStart,toEnd,domain->zoom_ni_srv)) ;
                       SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0);
                       CArray<double,1> lon=domain->lonvalue_srv(Range(0,domain->zoom_ni_srv-1)) ;
                       SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0);
                       break;
                   }

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0);
                     SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0);

                 SuperClassWriter::definition_start();

                 break;
              }
              case (ONE_FILE) :
              {
                 SuperClassWriter::addDimension(dimXid, domain->global_zoom_ni);
                 SuperClassWriter::addDimension(dimYid, domain->global_zoom_nj);

                 if (domain->hasBounds)
                   SuperClassWriter::addDimension(dimVertId, domain->nvertex);

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       dim0.push_back(dimYid); dim0.push_back(dimXid);
                       lonid = StdString("nav_lon").append(appendDomid);
                       latid = StdString("nav_lat").append(appendDomid);
                       SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                       SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                       break;

                     case CDomain::type_attr::rectilinear :
                       dim0.push_back(dimYid);
                       dim1.push_back(dimXid);
                       lonid = StdString("lon").append(appendDomid);
                       latid = StdString("lat").append(appendDomid);
                       SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                       SuperClassWriter::addVariable(lonid, NC_FLOAT, dim1);
                       break;
                   }

                   bounds_lonid = StdString("bounds_lon").append(appendDomid);
                   bounds_latid = StdString("bounds_lat").append(appendDomid);

                   this->writeAxisAttributes
                      (lonid, isRegularDomain ? "X" : "", "longitude", "Longitude", "degrees_east", domid);
                   this->writeAxisAttributes
                      (latid, isRegularDomain ? "Y" : "", "latitude", "Latitude", "degrees_north", domid);

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::addAttribute("bounds", bounds_lonid, &lonid);
                     SuperClassWriter::addAttribute("bounds", bounds_latid, &latid);

                     dim0.clear();
                     dim0.push_back(dimYid);
                     dim0.push_back(dimXid);
                     dim0.push_back(dimVertId);
                     SuperClassWriter::addVariable(bounds_lonid, NC_FLOAT, dim0);
                     SuperClassWriter::addVariable(bounds_latid, NC_FLOAT, dim0);
                   }
                 }

                 if (domain->hasArea)
                 {
                   dim0.clear();
                   dim0.push_back(dimYid); dim0.push_back(dimXid);
                   SuperClassWriter::addVariable(areaId, NC_FLOAT, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                   dim0.clear();
                 }

                 SuperClassWriter::definition_end();

                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                   {
                     std::vector<StdSize> start(2) ;
                     std::vector<StdSize> count(2) ;
                     if (domain->isEmpty())
                     {
                       start[0]=0 ; start[1]=0 ;
                       count[0]=0 ; count[1]=0 ;
                     }
                     else
                     {
                       start[1]=domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                       start[0]=domain->zoom_jbegin_srv-domain->global_zoom_jbegin;
                       count[1]=domain->zoom_ni_srv ; count[0]=domain->zoom_nj_srv ;
                     }

                     if (domain->hasLonLat)
                     {
                       SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                       SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);
                     }
                     break;
                   }
                   case CDomain::type_attr::rectilinear :
                   {
                     if (domain->hasLonLat)
                     {
                       std::vector<StdSize> start(1) ;
                       std::vector<StdSize> count(1) ;
                       if (domain->isEmpty())
                       {
                         start[0]=0 ;
                         count[0]=0 ;
                         SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                         SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);

                       }
                       else
                       {
                         start[0]=domain->zoom_jbegin_srv-domain->global_zoom_jbegin;
                         count[0]=domain->zoom_nj_srv ;
                         CArray<double,1> lat = domain->latvalue_srv(Range(fromStart,toEnd,domain->zoom_ni_srv)) ;
                         SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0,&start,&count);

                         start[0]=domain->zoom_ibegin_srv-domain->global_zoom_ibegin;
                         count[0]=domain->zoom_ni_srv ;
                         CArray<double,1> lon=domain->lonvalue_srv(Range(0,domain->zoom_ni_srv-1)) ;
                         SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0,&start,&count);
                       }
                     }
                     break;
                   }
                 }

                 if (domain->hasBounds)
                 {
                   std::vector<StdSize> start(3);
                   std::vector<StdSize> count(3);
                   if (domain->isEmpty())
                   {
                     start[2] = start[1] = start[0] = 0;
                     count[2] = count[1] = count[0] = 0;
                   }
                   else
                   {
                     start[2] = 0;
                     start[1] = domain->zoom_ibegin_srv - domain->global_zoom_ibegin;
                     start[0] = domain->zoom_jbegin_srv - domain->global_zoom_jbegin;
                     count[2] = domain->nvertex;
                     count[1] = domain->zoom_ni_srv;
                     count[0] = domain->zoom_nj_srv;
                   }

                   SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0, &start, &count);
                   SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0, &start, &count);
                 }

                 if (domain->hasArea)
                 {
                   std::vector<StdSize> start(2);
                   std::vector<StdSize> count(2);

                   if (domain->isEmpty())
                   {
                     start[0] = 0; start[1] = 0;
                     count[0] = 0; count[1] = 0;
                   }
                   else
                   {
                     start[1] = domain->zoom_ibegin_srv - domain->global_zoom_ibegin;
                     start[0] = domain->zoom_jbegin_srv - domain->global_zoom_jbegin;
                     count[1] = domain->zoom_ni_srv;
                     count[0] = domain->zoom_nj_srv;
                   }

                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0, &start, &count);
                 }

                 SuperClassWriter::definition_start();
                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the domain : ");
           msg.append(domid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeDomain_(CDomain* domain)", << msg);
         }

         domain->addRelFile(this->filename);
      }

      void CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)
      {
         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         std::vector<StdString> dim0, dim1;
         StdString domid     = (!domain->name.isEmpty())
                             ? domain->name.getValue() : domain->getId();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;


         StdString dimXid = StdString("cell").append(appendDomid);
         StdString dimVertId = StdString("nvertex").append(appendDomid);

         string lonid,latid,bounds_lonid,bounds_latid ;
         string areaId = "area" + appendDomid;

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->zoom_ni_srv);

                 lonid = StdString("lon").append(appendDomid);
                 latid = StdString("lat").append(appendDomid);
                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);
                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                   SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);
                   this->writeAxisAttributes(lonid, "", "longitude", "Longitude", "degrees_east", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                   this->writeAxisAttributes(latid, "", "latitude", "Latitude", "degrees_north", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                   if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, domain->nvertex);
                 }

                 dim0.clear();
                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, NC_FLOAT, dim0);
                   SuperClassWriter::addVariable(bounds_latid, NC_FLOAT, dim0);
                 }

                 dim0.clear();
                 dim0.push_back(dimXid);
                 if (domain->hasArea)
                 {
                   SuperClassWriter::addVariable(areaId, NC_FLOAT, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0);
                   SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0);
                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0);
                     SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0);

                 SuperClassWriter::definition_start();
                 break ;
              }

              case (ONE_FILE) :
              {
                 lonid = StdString("lon").append(appendDomid);
                 latid = StdString("lat").append(appendDomid);
                 bounds_lonid = StdString("bounds_lon").append(appendDomid);
                 bounds_latid = StdString("bounds_lat").append(appendDomid);
                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->nj_glo);
                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::addVariable(latid, NC_FLOAT, dim0);
                   SuperClassWriter::addVariable(lonid, NC_FLOAT, dim0);

                   this->writeAxisAttributes(lonid, "", "longitude", "Longitude", "degrees_east", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                   this->writeAxisAttributes(latid, "", "latitude", "Latitude", "degrees_north", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                   if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, domain->nvertex);
                 }
                 dim0.clear();

                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, NC_FLOAT, dim0);
                   SuperClassWriter::addVariable(bounds_latid, NC_FLOAT, dim0);
                 }

                 if (domain->hasArea)
                 {
                   dim0.clear();
                   dim0.push_back(dimXid);
                   SuperClassWriter::addVariable(areaId, NC_FLOAT, dim0);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 std::vector<StdSize> start(1), startBounds(2) ;
                 std::vector<StdSize> count(1), countBounds(2) ;
                 if (domain->isEmpty())
                 {
                   start[0]=0 ;
                   count[0]=0 ;
                   startBounds[1]=0 ;
                   countBounds[1]=domain->nvertex ;
                   startBounds[0]=0 ;
                   countBounds[0]=0 ;
                 }
                 else
                 {
                   start[0]=domain->zoom_jbegin_srv-domain->global_zoom_jbegin;
                   count[0]=domain->zoom_nj_srv ;
                   startBounds[0]=domain->zoom_jbegin_srv-domain->global_zoom_jbegin;
                   startBounds[1]=0 ;
                   countBounds[0]=domain->zoom_nj_srv ;
                   countBounds[1]=domain->nvertex ;
                 }

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::writeData(domain->latvalue_srv, latid, isCollective, 0,&start,&count);
                   SuperClassWriter::writeData(domain->lonvalue_srv, lonid, isCollective, 0,&start,&count);
                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lon_srv, bounds_lonid, isCollective, 0,&startBounds,&countBounds);
                     SuperClassWriter::writeData(domain->bounds_lat_srv, bounds_latid, isCollective, 0,&startBounds,&countBounds);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->area_srv, areaId, isCollective, 0, &start, &count);

                 SuperClassWriter::definition_start();

                 break;
              }
              default :
                 ERROR("CNc4DataOutput::writeDomain(domain)",
                       << "[ type = " << SuperClass::type << "]"
                       << " not implemented yet !");
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing the domain : ");
           msg.append(domid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)", << msg);
         }
         domain->addRelFile(this->filename);
      }
      //--------------------------------------------------------------

      void CNc4DataOutput::writeAxis_(CAxis* axis)
      {
        if (axis->IsWritten(this->filename)) return;
        axis->checkAttributes();
        int zoom_size_srv  = axis->zoom_size_srv;
        int zoom_begin_srv = axis->zoom_begin_srv;
        int zoom_size  = (MULTI_FILE == SuperClass::type) ? zoom_size_srv
                                                              : axis->global_zoom_size;
        int zoom_begin = (MULTI_FILE == SuperClass::type) ? zoom_begin_srv
                                                              : axis->global_zoom_begin;

        if ((0 == zoom_size_srv) && (MULTI_FILE == SuperClass::type)) return;

        std::vector<StdString> dims;
        StdString axisid = !axis->name.isEmpty() ? axis->name.getValue() : axis->getId();
        try
        {
          SuperClassWriter::addDimension(axisid, zoom_size);
          dims.push_back(axisid);
          SuperClassWriter::addVariable(axisid, NC_FLOAT, dims);

          if (!axis->name.isEmpty())
            SuperClassWriter::addAttribute("name", axis->name.getValue(), &axisid);

          if (!axis->standard_name.isEmpty())
            SuperClassWriter::addAttribute("standard_name", axis->standard_name.getValue(), &axisid);

          if (!axis->long_name.isEmpty())
            SuperClassWriter::addAttribute("long_name", axis->long_name.getValue(), &axisid);

          if (!axis->unit.isEmpty())
            SuperClassWriter::addAttribute("units", axis->unit.getValue(), &axisid);

          if (!axis->positive.isEmpty())
          {
            SuperClassWriter::addAttribute("axis", string("Z"), &axisid);
            SuperClassWriter::addAttribute("positive",
                                           (axis->positive == CAxis::positive_attr::up) ? string("up") : string("down"),
                                           &axisid);
          }

          StdString axisBoundsId = axisid + "_bounds";
          if (!axis->bounds.isEmpty())
          {
            dims.push_back("axis_nbounds");
            SuperClassWriter::addVariable(axisBoundsId, NC_FLOAT, dims);
            SuperClassWriter::addAttribute("bounds", axisBoundsId, &axisid);
          }

          SuperClassWriter::definition_end();
          switch (SuperClass::type)
          {
            case MULTI_FILE:
            {
              CArray<double,1> axis_value(zoom_size_srv);
              for (int i = 0; i < zoom_size_srv; i++) axis_value(i) = axis->value_srv(i);
              SuperClassWriter::writeData(axis_value, axisid, isCollective, 0);

              if (!axis->bounds.isEmpty())
              {
                CArray<double,2> axisBounds(2, zoom_size_srv);
                for (int i = 0; i < zoom_size_srv; i++)
                {
                  axisBounds(0, i) = axis->bound_srv(i, 0);
                  axisBounds(1, i) = axis->bound_srv(i, 1);
                }
                SuperClassWriter::writeData(axisBounds, axisBoundsId, isCollective, 0);
              }

              SuperClassWriter::definition_start();

              break;
            }
            case ONE_FILE:
            {
              CArray<double,1> axis_value(zoom_size_srv);
              axis_value = axis->value_srv;

              std::vector<StdSize> start(1) ;
              std::vector<StdSize> count(1) ;
              start[0] = zoom_begin_srv-axis->global_zoom_begin;
              count[0] = zoom_size_srv;
              SuperClassWriter::writeData(axis_value, axisid, isCollective, 0, &start, &count);

              if (!axis->bounds.isEmpty())
              {
                CArray<double,2> axisBounds(2, zoom_size_srv);
                for (int i = 0; i < zoom_size_srv; ++i)
                {
                  axisBounds(0, i+zoom_begin_srv) = axis->bound_srv(i, 0);
                  axisBounds(1, i+zoom_begin_srv) = axis->bound_srv(i, 1);
                }
                SuperClassWriter::writeData(axisBounds, axisBoundsId, isCollective, 0, &start, &count);
              }

              SuperClassWriter::definition_start();

              break;
            }
            default :
              ERROR("CNc4DataOutput::writeDomain(domain)",
                    << "[ type = " << SuperClass::type << "]"
                    << " not implemented yet !");
          }
        }
        catch (CNetCdfException& e)
        {
          StdString msg("On writing the axis : ");
          msg.append(axisid); msg.append("\n");
          msg.append("In the context : ");
          CContext* context = CContext::getCurrent() ;
          msg.append(context->getId()); msg.append("\n");
          msg.append(e.what());
          ERROR("CNc4DataOutput::writeAxis_(CAxis* axis)", << msg);
        }
        axis->addRelFile(this->filename);
     }

     //--------------------------------------------------------------

     void CNc4DataOutput::writeGridCompressed_(CGrid* grid)
     {
       if (grid->isScalarGrid() || grid->isWrittenCompressed(this->filename)) return;

       try
       {
         CArray<bool,1> axisDomainOrder = grid->axis_domain_order;
         std::vector<StdString> domainList = grid->getDomainList();
         std::vector<StdString> axisList   = grid->getAxisList();
         int numElement = axisDomainOrder.numElements(), idxDomain = 0, idxAxis = 0;

         std::vector<StdString> dims;

         if (grid->isCompressible())
         {
           StdString varId = grid->getId() + "_points";

           int nbIndexes = (SuperClass::type == MULTI_FILE) ? grid->getNumberWrittenIndexes() : grid->getTotalNumberWrittenIndexes();
           SuperClassWriter::addDimension(varId, nbIndexes);

           dims.push_back(varId);
           SuperClassWriter::addVariable(varId, NC_INT, dims);

           StdOStringStream compress;
           for (int i = numElement - 1; i >= 0; --i)
           {
             if (axisDomainOrder(i))
             {
               CDomain* domain = CDomain::get(domainList[domainList.size() - idxDomain - 1]);
               StdString domId = !domain->name.isEmpty() ? domain->name.getValue() : domain->getId();
               StdString appendDomId  = singleDomain ? "" : "_" + domId;

               switch (domain->type)
               {
                 case CDomain::type_attr::curvilinear:
                   compress << "y" << appendDomId << " x" << appendDomId;
                   break;
                 case CDomain::type_attr::rectilinear:
                   compress << "lat" << appendDomId << " lon" << appendDomId;
                   break;
                 case CDomain::type_attr::unstructured:
                   compress << "cell" << appendDomId;
                   break;
               }
               ++idxDomain;
             }
             else
             {
               CAxis* axis = CAxis::get(axisList[axisList.size() - idxAxis - 1]);
               compress << (!axis->name.isEmpty() ? axis->name.getValue() : axis->getId());
               ++idxAxis;
             }

             if (i != 0) compress << ' ';
           }
           SuperClassWriter::addAttribute("compress", compress.str(), &varId);

           grid->computeCompressedIndex();

           CArray<int, 1> indexes(grid->getNumberWrittenIndexes());
           std::map<int, CArray<size_t, 1> >::const_iterator it;
           for (it = grid->outIndexFromClient.begin(); it != grid->outIndexFromClient.end(); ++it)
           {
             const CArray<size_t, 1> compressedIndexes = grid->compressedOutIndexFromClient[it->first];
             for (int i = 0; i < it->second.numElements(); i++)
               indexes(compressedIndexes(i)) = it->second(i);
           }

           switch (SuperClass::type)
           {
             case (MULTI_FILE):
             {
               SuperClassWriter::writeData(indexes, varId, isCollective, 0);
               break;
             }
             case (ONE_FILE):
             {
               if (grid->doGridHaveDataDistributed())
                 grid->getDistributionServer()->computeGlobalIndex(indexes);

               std::vector<StdSize> start, count;
               start.push_back(grid->getOffsetWrittenIndexes());
               count.push_back(grid->getNumberWrittenIndexes());

               SuperClassWriter::writeData(indexes, varId, isCollective, 0, &start, &count);
               break;
             }
           }
         }
         else
         {
           for (int i = 0; i < numElement; ++i)
           {
             StdString varId, compress;
             CArray<int, 1> indexes;
             bool isDistributed;
             StdSize nbIndexes, totalNbIndexes, offset;
             int firstGlobalIndex;

             if (axisDomainOrder(i))
             {
               CDomain* domain = CDomain::get(domainList[idxDomain]);
               if (!domain->isCompressible()
                    || domain->type == CDomain::type_attr::unstructured
                    || domain->isWrittenCompressed(this->filename))
                 continue;

               StdString domId = !domain->name.isEmpty() ? domain->name.getValue() : domain->getId();
               StdString appendDomId  = singleDomain ? "" : "_" + domId;

               varId = domId + "_points";
               switch (domain->type)
               {
                 case CDomain::type_attr::curvilinear:
                   compress = "y" + appendDomId + " x" + appendDomId;
                   break;
                 case CDomain::type_attr::rectilinear:
                   compress = "lat" + appendDomId + " lon" + appendDomId;
                   break;
               }

               const std::vector<int>& indexesToWrite = domain->getIndexesToWrite();
               indexes.resize(indexesToWrite.size());
               for (int n = 0; n < indexes.numElements(); ++n)
                 indexes(n) = indexesToWrite[n];

               isDistributed = domain->isDistributed();
               nbIndexes = domain->getNumberWrittenIndexes();
               totalNbIndexes = domain->getTotalNumberWrittenIndexes();
               offset = domain->getOffsetWrittenIndexes();
               firstGlobalIndex = domain->ibegin + domain->jbegin * domain->ni_glo;

               domain->addRelFileCompressed(this->filename);
               ++idxDomain;
             }
             else
             {
               CAxis* axis = CAxis::get(axisList[idxAxis]);
               if (!axis->isCompressible() || axis->isWrittenCompressed(this->filename))
                 continue;

               StdString axisId = !axis->name.isEmpty() ? axis->name.getValue() : axis->getId();
               varId = axisId + "_points";
               compress = axisId;

               const std::vector<int>& indexesToWrite = axis->getIndexesToWrite();
               indexes.resize(indexesToWrite.size());
               for (int n = 0; n < indexes.numElements(); ++n)
                 indexes(n) = indexesToWrite[n];

               isDistributed = axis->isDistributed();
               nbIndexes = axis->getNumberWrittenIndexes();
               totalNbIndexes = axis->getTotalNumberWrittenIndexes();
               offset = axis->getOffsetWrittenIndexes();
               firstGlobalIndex = axis->begin;

               axis->addRelFileCompressed(this->filename);
               ++idxAxis;
             }

             if (!varId.empty())
             {
               SuperClassWriter::addDimension(varId, (SuperClass::type == MULTI_FILE) ? nbIndexes : totalNbIndexes);

               dims.clear();
               dims.push_back(varId);
               SuperClassWriter::addVariable(varId, NC_INT, dims);

               SuperClassWriter::addAttribute("compress", compress, &varId);

               switch (SuperClass::type)
               {
                 case (MULTI_FILE):
                 {
                   indexes -= firstGlobalIndex;
                   SuperClassWriter::writeData(indexes, varId, isCollective, 0);
                   break;
                 }
                 case (ONE_FILE):
                 {
                   std::vector<StdSize> start, count;
                   start.push_back(offset);
                   count.push_back(nbIndexes);

                   SuperClassWriter::writeData(indexes, varId, isCollective, 0, &start, &count);
                   break;
                 }
               }
             }
           }

           if (!dims.empty())
             grid->computeCompressedIndex();
         }

         grid->addRelFileCompressed(this->filename);
       }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing compressed grid : ");
         msg.append(grid->getId()); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent();
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeGridCompressed_(CGrid* grid)", << msg);
       }
     }

     //--------------------------------------------------------------

     void CNc4DataOutput::writeTimeDimension_(void)
     {
       try
       {
        SuperClassWriter::addDimension("time_counter");
       }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing time dimension : time_couter\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeTimeDimension_(void)", << msg);
       }
     }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeField_(CField* field)
      {
         CContext* context = CContext::getCurrent() ;
         CContextServer* server=context->server ;

         std::vector<StdString> dims, coodinates;
         CGrid* grid = field->grid;
         if (!grid->doGridHaveDataToWrite())
          if (SuperClass::type==MULTI_FILE) return ;

         CArray<bool,1> axisDomainOrder = grid->axis_domain_order;
         int numElement = axisDomainOrder.numElements(), idxDomain = 0, idxAxis = 0;
         std::vector<StdString> domainList = grid->getDomainList();
         std::vector<StdString> axisList   = grid->getAxisList();

         StdString timeid  = StdString("time_counter");
         StdString dimXid,dimYid;
         std::deque<StdString> dimIdList, dimCoordList;
         bool hasArea = false;
         StdString cellMeasures = "area:";
         bool compressedOutput = !field->indexed_output.isEmpty() && field->indexed_output;

         for (int i = 0; i < numElement; ++i)
         {
           if (axisDomainOrder(i))
           {
             CDomain* domain = CDomain::get(domainList[idxDomain]);
             StdString domId = !domain->name.isEmpty() ? domain->name.getValue() : domain->getId();
             StdString appendDomId  = singleDomain ? "" : "_" + domId ;

             if (compressedOutput && domain->isCompressible() && domain->type != CDomain::type_attr::unstructured)
             {
               dimIdList.push_back(domId + "_points");
               field->setUseCompressedOutput();
             }

             switch (domain->type)
             {
               case CDomain::type_attr::curvilinear:
                 if (!compressedOutput || !domain->isCompressible())
                 {
                   dimXid     = StdString("x").append(appendDomId);
                   dimIdList.push_back(dimXid);
                   dimYid     = StdString("y").append(appendDomId);
                   dimIdList.push_back(dimYid);
                 }
                 dimCoordList.push_back(StdString("nav_lon").append(appendDomId));
                 dimCoordList.push_back(StdString("nav_lat").append(appendDomId));
                 break ;
               case CDomain::type_attr::rectilinear:
                 if (!compressedOutput || !domain->isCompressible())
                 {
                   dimXid     = StdString("lon").append(appendDomId);
                   dimIdList.push_back(dimXid);
                   dimYid     = StdString("lat").append(appendDomId);
                   dimIdList.push_back(dimYid);
                 }
                 break ;
               case CDomain::type_attr::unstructured:
                 dimXid     = StdString("cell").append(appendDomId);
                 dimIdList.push_back(dimXid);
                 dimCoordList.push_back(StdString("lon").append(appendDomId));
                 dimCoordList.push_back(StdString("lat").append(appendDomId));
                 break ;
             }
             if (domain->hasArea)
             {
               hasArea = true;
               cellMeasures += " area" + appendDomId;
             }
             ++idxDomain;
           }
           else
           {
             CAxis* axis = CAxis::get(axisList[idxAxis]);
             StdString axisId = !axis->name.isEmpty() ? axis->name.getValue() : axis->getId();

             if (compressedOutput && axis->isCompressible())
             {
               dimIdList.push_back(axisId + "_points");
               field->setUseCompressedOutput();
             }
             else
               dimIdList.push_back(axisId);

             dimCoordList.push_back(axisId);
             ++idxAxis;
           }
         }

/*
         StdString lonid_loc = (server->intraCommSize > 1)
                             ? StdString("lon").append(appendDomid).append("_local")
                             : lonid;
         StdString latid_loc = (server->intraCommSize > 1)
                             ? StdString("lat").append(appendDomid).append("_local")
                             : latid;
*/
         StdString fieldid   = (!field->name.isEmpty())
                             ? field->name.getValue() : field->getBaseFieldReference()->getId();

//         unsigned int ssize = domain->zoom_ni_loc.getValue() * domain->zoom_nj_loc.getValue();
//         bool isCurvilinear = (domain->lonvalue.getValue()->size() == ssize);
//          bool isCurvilinear = domain->isCurvilinear ;

         nc_type type ;
         if (field->prec.isEmpty()) type =  NC_FLOAT ;
         else
         {
           if (field->prec==2) type = NC_SHORT ;
           else if (field->prec==4)  type =  NC_FLOAT ;
           else if (field->prec==8)   type =  NC_DOUBLE ;
         }

         bool wtime   = !(!field->operation.isEmpty() && field->getOperationTimeType() == func::CFunctor::once);

         if (wtime)
         {

            //StdOStringStream oss;
           // oss << "time_" << field->operation.getValue()
           //     << "_" << field->getRelFile()->output_freq.getValue();
          //oss
            if (field->getOperationTimeType() == func::CFunctor::instant) coodinates.push_back(string("time_instant"));
            else if (field->getOperationTimeType() == func::CFunctor::centered) coodinates.push_back(string("time_centered"));
            dims.push_back(timeid);
         }

         if (compressedOutput && grid->isCompressible())
         {
           dims.push_back(grid->getId() + "_points");
           field->setUseCompressedOutput();
         }
         else
         {
           while (!dimIdList.empty())
           {
             dims.push_back(dimIdList.back());
             dimIdList.pop_back();
           }
         }

         while (!dimCoordList.empty())
         {
           coodinates.push_back(dimCoordList.back());
           dimCoordList.pop_back();
         }

         try
         {
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

            if (!field->valid_min.isEmpty())
              SuperClassWriter::addAttribute
                 ("valid_min", field->valid_min.getValue(), &fieldid);

           if (!field->valid_max.isEmpty())
              SuperClassWriter::addAttribute
                 ("valid_max", field->valid_max.getValue(), &fieldid);

            if (!field->scale_factor.isEmpty())
              SuperClassWriter::addAttribute
                 ("scale_factor", field->scale_factor.getValue(), &fieldid);

             if (!field->add_offset.isEmpty())
              SuperClassWriter::addAttribute
                 ("add_offset", field->add_offset.getValue(), &fieldid);

           SuperClassWriter::addAttribute
                 ("online_operation", field->operation.getValue(), &fieldid);

          // write child variables as attributes


           vector<CVariable*> listVars = field->getAllVariables() ;
           for (vector<CVariable*>::iterator it = listVars.begin() ;it != listVars.end(); it++) writeAttribute_(*it, fieldid) ;


           if (wtime)
           {
              CDuration freqOp = field->freq_op.getValue();
              freqOp.solveTimeStep(*context->calendar);
              StdString freqOpStr = freqOp.toStringUDUnits();
              SuperClassWriter::addAttribute("interval_operation", freqOpStr, &fieldid);

              CDuration freqOut = field->getRelFile()->output_freq.getValue();
              freqOut.solveTimeStep(*context->calendar);
              SuperClassWriter::addAttribute("interval_write", freqOut.toStringUDUnits(), &fieldid);

              StdString cellMethods = "time: ";
              if (field->operation.getValue() == "instant") cellMethods += "point";
              else if (field->operation.getValue() == "average") cellMethods += "mean";
              else if (field->operation.getValue() == "accumulate") cellMethods += "sum";
              else cellMethods += field->operation;
              if (freqOp.resolve(*context->calendar) != freqOut.resolve(*context->calendar))
                cellMethods += " (interval: " + freqOpStr + ")";
              SuperClassWriter::addAttribute("cell_methods", cellMethods, &fieldid);
           }

           if (hasArea)
             SuperClassWriter::addAttribute("cell_measures", cellMeasures, &fieldid);

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
              SuperClassWriter::setDefaultValue(fieldid, (double*)NULL);

            if (field->compression_level.isEmpty())
              field->compression_level = field->file->compression_level.isEmpty() ? 0 : field->file->compression_level;
            SuperClassWriter::setCompressionLevel(fieldid, field->compression_level);

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
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing field : ");
           msg.append(fieldid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeField_(CField* field)", << msg);
         }
      }

      //--------------------------------------------------------------

      void CNc4DataOutput::writeFile_ (CFile* file)
      {
         StdString filename = (!file->name.isEmpty())
                            ? file->name.getValue() : file->getId();
         StdString description = (!file->description.isEmpty())
                               ? file->description.getValue()
                               : StdString("Created by xios");

         singleDomain = (file->nbDomains == 1);

         try
         {
           this->writeFileAttributes(filename, description,
                                     StdString("CF-1.5"),
                                     StdString("An IPSL model"),
                                     this->getTimeStamp());

           SuperClassWriter::addDimension("axis_nbounds", 2);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing file : ");
           msg.append(filename); msg.append("\n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeFile_ (CFile* file)", << msg);
         }
      }

      void CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)
      {
        string name ;
        if (!var->name.isEmpty()) name=var->name ;
        else if (var->hasId()) name=var->getId() ;
        else return ;

        try
        {
          if (var->type.getValue() == CVariable::type_attr::t_int || var->type.getValue() == CVariable::type_attr::t_int32)
            addAttribute(name, var->getData<int>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_int16)
            addAttribute(name, var->getData<short int>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_float)
            addAttribute(name, var->getData<float>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_double)
            addAttribute(name, var->getData<double>(), &fieldId);
          else if (var->type.getValue() == CVariable::type_attr::t_string)
            addAttribute(name, var->getData<string>(), &fieldId);
          else
            ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)",
                  << "Unsupported variable of type " << var->type.getStringValue());
        }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing attributes of variable with name : ");
         msg.append(name); msg.append("in the field "); msg.append(fieldId); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var, const string& fieldId)", << msg);
       }
     }

     void CNc4DataOutput::writeAttribute_ (CVariable* var)
     {
        string name ;
        if (!var->name.isEmpty()) name=var->name ;
        else if (var->hasId()) name=var->getId() ;
        else return ;
        try
        {
          if (var->type.getValue() == CVariable::type_attr::t_int || var->type.getValue() == CVariable::type_attr::t_int32)
            addAttribute(name, var->getData<int>());
          else if (var->type.getValue() == CVariable::type_attr::t_int16)
            addAttribute(name, var->getData<short int>());
          else if (var->type.getValue() == CVariable::type_attr::t_float)
            addAttribute(name, var->getData<float>());
          else if (var->type.getValue() == CVariable::type_attr::t_double)
            addAttribute(name, var->getData<double>());
          else if (var->type.getValue() == CVariable::type_attr::t_string)
            addAttribute(name, var->getData<string>());
          else
            ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var)",
                  << "Unsupported variable of type " << var->type.getStringValue());
        }
       catch (CNetCdfException& e)
       {
         StdString msg("On writing attributes of variable with name : ");
         msg.append(name); msg.append("\n");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::writeAttribute_ (CVariable* var)", << msg);
       }
     }

      void CNc4DataOutput::syncFile_ (void)
      {
        try
        {
          SuperClassWriter::sync() ;
        }
        catch (CNetCdfException& e)
        {
         StdString msg("On synchronizing the write among processes");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::syncFile_ (void)", << msg);
        }
      }

      void CNc4DataOutput::closeFile_ (void)
      {
        try
        {
          SuperClassWriter::close() ;
        }
        catch (CNetCdfException& e)
        {
         StdString msg("On closing file");
         msg.append("In the context : ");
         CContext* context = CContext::getCurrent() ;
         msg.append(context->getId()); msg.append("\n");
         msg.append(e.what());
         ERROR("CNc4DataOutput::syncFile_ (void)", << msg);
        }

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

      void CNc4DataOutput::writeFieldData_ (CField*  field)
      {
         CContext* context = CContext::getCurrent() ;
//          if (field->getRelFile()->isSyncTime()) SuperClassWriter::sync() ;
         CContextServer* server=context->server ;

         CGrid* grid = field->grid ;

         if (!grid->doGridHaveDataToWrite())
          if (SuperClass::type==MULTI_FILE || !isCollective) return ;

         StdString fieldid   = (!field->name.isEmpty())
                             ? field->name.getValue()
                             : field->getBaseFieldReference()->getId();

         StdOStringStream oss;
         string timeAxisId ;
         if (field->getOperationTimeType() == func::CFunctor::instant)  timeAxisId="time_instant" ;
         else if (field->getOperationTimeType() == func::CFunctor::centered)  timeAxisId="time_centered" ;

         StdString timeBoundId("time_counter_bounds");

         StdString timeAxisBoundId;
         if (field->getOperationTimeType() == func::CFunctor::instant)  timeAxisBoundId="time_instant_bounds" ;
         else if (field->getOperationTimeType() == func::CFunctor::centered)  timeAxisBoundId="time_centered_bounds" ;

         CArray<double,1> time_data(1) ;
         CArray<double,1> time_data_bound(2);
         CArray<double,1> time_counter(1) ;
         CArray<double,1> time_counter_bound(2);

        bool wtime   = !(!field->operation.isEmpty() && (field->getOperationTimeType() == func::CFunctor::once));

        if (wtime)
        {
          Time lastWrite = field->last_Write_srv;
          Time lastLastWrite = field->lastlast_Write_srv;

          if (field->getOperationTimeType() == func::CFunctor::instant)
            time_data(0) = lastWrite;
          else if (field->getOperationTimeType() == func::CFunctor::centered)
            time_data(0) = (lastWrite + lastLastWrite) / 2;

          if (field->getOperationTimeType() == func::CFunctor::instant)
            time_data_bound(0) = time_data_bound(1) = lastWrite;
          else if (field->getOperationTimeType() == func::CFunctor::centered)
          {
            time_data_bound(0) = lastLastWrite;
            time_data_bound(1) = lastWrite;
          }

          if (field->file->time_counter == CFile::time_counter_attr::instant)
            time_counter(0) = lastWrite;
          else if (field->file->time_counter == CFile::time_counter_attr::centered)
            time_counter(0) = (lastWrite + lastLastWrite) / 2;
          else if (field->file->time_counter == CFile::time_counter_attr::record)
            time_counter(0) = field->getNStep() - 1;


          if (field->file->time_counter == CFile::time_counter_attr::instant)
            time_counter_bound(0) = time_counter_bound(1) = lastWrite;
          else if (field->file->time_counter == CFile::time_counter_attr::centered)
          {
            time_counter_bound(0) = lastLastWrite;
            time_counter_bound(1) = lastWrite;
          }
          else if (field->file->time_counter == CFile::time_counter_attr::record)
            time_counter_bound(0) = time_counter_bound(1) = field->getNStep() - 1;
        }

         bool isRoot ;
         if (server->intraCommRank==0) isRoot=true ;
         else isRoot=false ;

         if (!field->scale_factor.isEmpty() || !field->add_offset.isEmpty())
         {
           double scaleFactor=1. ;
           double addOffset=0. ;
           if (!field->scale_factor.isEmpty()) scaleFactor=field->scale_factor ;
           if (!field->add_offset.isEmpty()) addOffset=field->add_offset ;
           field->scaleFactorAddOffset(scaleFactor,addOffset) ;
         }

         try
         {
           size_t writtenSize;
           if (field->getUseCompressedOutput())
             writtenSize = grid->getNumberWrittenIndexes();
           else
             writtenSize = grid->getWrittenDataSize();

           CArray<double,1> fieldData(writtenSize);
           if (!field->default_value.isEmpty()) fieldData = field->default_value;

           if (field->getUseCompressedOutput())
             field->outputCompressedField(fieldData);
           else
             field->outputField(fieldData);

           if (!field->prec.isEmpty() && field->prec==2) fieldData=round(fieldData) ;

           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 SuperClassWriter::writeData(fieldData, fieldid, isCollective, field->getNStep() - 1);
                 if (wtime)
                 {
                   SuperClassWriter::writeData(time_data, timeAxisId, isCollective, field->getNStep() - 1);
                   SuperClassWriter::writeData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep() - 1);
                   if (field->file->time_counter != CFile::time_counter_attr::none)
                   {
                     SuperClassWriter::writeData(time_counter, string("time_counter"), isCollective, field->getNStep() - 1);
                     if (field->file->time_counter != CFile::time_counter_attr::record)
                       SuperClassWriter::writeData(time_counter_bound, timeBoundId, isCollective, field->getNStep() - 1);
                   }
                 }
                 break ;
              }
              case (ONE_FILE) :
              {
                const std::vector<int>& nZoomBeginGlobal = grid->getDistributionServer()->getZoomBeginGlobal();
                const std::vector<int>& nZoomBeginServer = grid->getDistributionServer()->getZoomBeginServer();
                const std::vector<int>& nZoomSizeServer  = grid->getDistributionServer()->getZoomSizeServer();

                std::vector<StdSize> start, count;

                if (field->getUseCompressedOutput())
                {
                  if (grid->isCompressible())
                  {
                    start.push_back(grid->getOffsetWrittenIndexes());
                    count.push_back(grid->getNumberWrittenIndexes());
                  }
                  else
                  {
                    CArray<bool,1> axisDomainOrder = grid->axis_domain_order;
                    std::vector<StdString> domainList = grid->getDomainList();
                    std::vector<StdString> axisList   = grid->getAxisList();
                    int numElement = axisDomainOrder.numElements();
                    int idxDomain = domainList.size() - 1, idxAxis = axisList.size() - 1;
                    int idx = nZoomBeginGlobal.size() - 1;

                    start.reserve(nZoomBeginGlobal.size());
                    count.reserve(nZoomBeginGlobal.size());


                    for (int i = numElement - 1; i >= 0; --i)
                    {
                      if (axisDomainOrder(i))
                      {
                        CDomain* domain = CDomain::get(domainList[idxDomain]);

                        if (domain->isCompressible())
                        {
                          start.push_back(domain->getOffsetWrittenIndexes());
                          count.push_back(domain->getNumberWrittenIndexes());
                        }
                        else
                        {
                          start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                          count.push_back(nZoomSizeServer[idx]);
                          start.push_back(nZoomBeginServer[idx - 1] - nZoomBeginGlobal[idx - 1]);
                          count.push_back(nZoomSizeServer[idx - 1]);
                        }

                        --idxDomain;
                        idx -= 2;
                      }
                      else
                      {
                        CAxis* axis = CAxis::get(axisList[idxAxis]);

                        if (axis->isCompressible())
                        {
                          start.push_back(axis->getOffsetWrittenIndexes());
                          count.push_back(axis->getNumberWrittenIndexes());
                        }
                        else
                        {
                          start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
                          count.push_back(nZoomSizeServer[idx]);
                        }

                        --idxAxis;
                        --idx;
                      }
                    }
                  }
                }
                else
                {
                  int ssize = nZoomBeginGlobal.size();

                  start.resize(ssize);
                  count.resize(ssize);

                  for (int i = 0; i < ssize; ++i)
                  {
                    start[i] = nZoomBeginServer[ssize - i - 1] - nZoomBeginGlobal[ssize - i - 1];
                    count[i] = nZoomSizeServer[ssize - i - 1];
                  }
                }

                SuperClassWriter::writeData(fieldData, fieldid, isCollective, field->getNStep() - 1, &start, &count);
                if (wtime)
                {
                   SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, field->getNStep() - 1, isRoot);
                   SuperClassWriter::writeTimeAxisData(time_data_bound, timeAxisBoundId, isCollective, field->getNStep() - 1, isRoot);
                   if (field->file->time_counter != CFile::time_counter_attr::none)
                   {
                     SuperClassWriter::writeTimeAxisData(time_counter, string("time_counter"), isCollective, field->getNStep() - 1, isRoot);
                     if (field->file->time_counter != CFile::time_counter_attr::record)
                       SuperClassWriter::writeTimeAxisData(time_counter_bound, timeBoundId, isCollective, field->getNStep() - 1, isRoot);
                   }
                }

                break;
              }
            }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing field data: ");
           msg.append(fieldid); msg.append("\n");
           msg.append("In the context : ");
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeFieldData_ (CField*  field)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxis_
                  (CField*    field,
                   const boost::shared_ptr<CCalendar> cal)
      {
         StdOStringStream oss;

         if (field->getOperationTimeType() == func::CFunctor::once) return ;

//         oss << "time_" << field->operation.getValue()
//             << "_" << field->getRelFile()->output_freq.getValue();

//         StdString axisid = oss.str();
//         if (field->getOperationTimeType() == func::CFunctor::centered) axisid="time_centered" ;
//         else if (field->getOperationTimeType() == func::CFunctor::instant) axisid="time_instant" ;

         StdString axisid("time_centered") ;
         StdString axisBoundId("time_centered_bounds");
         StdString timeid("time_counter");
         StdString timeBoundId("axis_nbounds");

         if (field->getOperationTimeType() == func::CFunctor::instant)
         {
            axisid = "time_instant";
            axisBoundId = "time_instant_bounds";
         }

         try
         {
          // Adding time_instant or time_centered
           std::vector<StdString> dims;
           dims.push_back(timeid);
           if (!SuperClassWriter::varExist(axisid))
           {
              SuperClassWriter::addVariable(axisid, NC_DOUBLE, dims);

              CDate timeOrigin=cal->getTimeOrigin() ;
              StdOStringStream oss2;
  //            oss2<<initDate.getYear()<<"-"<<initDate.getMonth()<<"-"<<initDate.getDay()<<" "
  //                <<initDate.getHour()<<"-"<<initDate.getMinute()<<"-"<<initDate.getSecond() ;
              StdString strInitdate=oss2.str() ;
              StdString strTimeOrigin=timeOrigin.toString() ;
              this->writeTimeAxisAttributes
                 (axisid, cal->getType(),
                  StdString("seconds since ").append(strTimeOrigin),
                  strTimeOrigin, axisBoundId);
           }

           // Adding time_instant_bounds or time_centered_bounds variables
           if (!SuperClassWriter::varExist(axisBoundId))
           {
              dims.clear() ;
              dims.push_back(timeid);
              dims.push_back(timeBoundId);
              SuperClassWriter::addVariable(axisBoundId, NC_DOUBLE, dims);
           }

           if (field->file->time_counter != CFile::time_counter_attr::none)
           {
             // Adding time_counter
             axisid = "time_counter";
             axisBoundId = "time_counter_bounds";
             dims.clear();
             dims.push_back(timeid);
             if (!SuperClassWriter::varExist(axisid))
             {
                SuperClassWriter::addVariable(axisid, NC_DOUBLE, dims);
                SuperClassWriter::addAttribute("axis", string("T"), &axisid);

                if (field->file->time_counter != CFile::time_counter_attr::record)
                {
                  CDate timeOrigin = cal->getTimeOrigin();
                  StdString strTimeOrigin = timeOrigin.toString();

                  this->writeTimeAxisAttributes(axisid, cal->getType(),
                                                StdString("seconds since ").append(strTimeOrigin),
                                                strTimeOrigin, axisBoundId);
                }
             }

             // Adding time_counter_bound dimension
             if (field->file->time_counter != CFile::time_counter_attr::record)
             {
                if (!SuperClassWriter::varExist(axisBoundId))
                {
                  dims.clear();
                  dims.push_back(timeid);
                  dims.push_back(timeBoundId);
                  SuperClassWriter::addVariable(axisBoundId, NC_DOUBLE, dims);
                }
             }
           }
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing time axis data: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeTimeAxis_ (CField*    field, \
                  const boost::shared_ptr<CCalendar> cal)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name,
                                                   const StdString & calendar,
                                                   const StdString & units,
                                                   const StdString & time_origin,
                                                   const StdString & time_bounds,
                                                   const StdString & standard_name,
                                                   const StdString & long_name)
      {
         try
         {
           SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
           SuperClassWriter::addAttribute("long_name",     long_name    , &axis_name);
           SuperClassWriter::addAttribute("calendar",      calendar     , &axis_name);
           SuperClassWriter::addAttribute("units",         units        , &axis_name);
           SuperClassWriter::addAttribute("time_origin",   time_origin  , &axis_name);
           SuperClassWriter::addAttribute("bounds",        time_bounds  , &axis_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing time axis Attribute: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeTimeAxisAttributes(const StdString & axis_name, \
                                                          const StdString & calendar,\
                                                          const StdString & units, \
                                                          const StdString & time_origin, \
                                                          const StdString & time_bounds, \
                                                          const StdString & standard_name, \
                                                          const StdString & long_name)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeAxisAttributes(const StdString & axis_name,
                                               const StdString & axis,
                                               const StdString & standard_name,
                                               const StdString & long_name,
                                               const StdString & units,
                                               const StdString & nav_model)
      {
         try
         {
          if (!axis.empty())
            SuperClassWriter::addAttribute("axis"       , axis         , &axis_name);

          SuperClassWriter::addAttribute("standard_name", standard_name, &axis_name);
          SuperClassWriter::addAttribute("long_name"    , long_name    , &axis_name);
          SuperClassWriter::addAttribute("units"        , units        , &axis_name);
          SuperClassWriter::addAttribute("nav_model"    , nav_model    , &axis_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Axis Attribute: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeAxisAttributes(const StdString & axis_name, \
                                                      const StdString & axis, \
                                                      const StdString & standard_name, \
                                                      const StdString & long_name, \
                                                      const StdString & units, \
                                                      const StdString & nav_model)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeLocalAttributes
         (int ibegin, int ni, int jbegin, int nj, StdString domid)
      {
        try
        {
         SuperClassWriter::addAttribute(StdString("ibegin").append(domid), ibegin);
         SuperClassWriter::addAttribute(StdString("ni"    ).append(domid), ni);
         SuperClassWriter::addAttribute(StdString("jbegin").append(domid), jbegin);
         SuperClassWriter::addAttribute(StdString("nj"    ).append(domid), nj);
        }
        catch (CNetCdfException& e)
        {
           StdString msg("On writing Local Attributes: ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeLocalAttributes \
                  (int ibegin, int ni, int jbegin, int nj, StdString domid)", << msg);
        }

      }

      void CNc4DataOutput::writeLocalAttributes_IOIPSL(const StdString& dimXid, const StdString& dimYid,
                                                       int ibegin, int ni, int jbegin, int nj, int ni_glo, int nj_glo, int rank, int size)
      {
         CArray<int,1> array(2) ;

         try
         {
           SuperClassWriter::addAttribute("DOMAIN_number_total",size ) ;
           SuperClassWriter::addAttribute("DOMAIN_number", rank) ;
           array = SuperClassWriter::getDimension(dimXid) + 1, SuperClassWriter::getDimension(dimYid) + 1;
           SuperClassWriter::addAttribute("DOMAIN_dimensions_ids",array) ;
           array=ni_glo,nj_glo ;
           SuperClassWriter::addAttribute("DOMAIN_size_global", array) ;
           array=ni,nj ;
           SuperClassWriter::addAttribute("DOMAIN_size_local", array) ;
           array=ibegin,jbegin ;
           SuperClassWriter::addAttribute("DOMAIN_position_first", array) ;
           array=ibegin+ni-1,jbegin+nj-1 ;
           SuperClassWriter::addAttribute("DOMAIN_position_last",array) ;
           array=0,0 ;
           SuperClassWriter::addAttribute("DOMAIN_halo_size_start", array) ;
           SuperClassWriter::addAttribute("DOMAIN_halo_size_end", array);
           SuperClassWriter::addAttribute("DOMAIN_type",string("box")) ;
  /*
           SuperClassWriter::addAttribute("DOMAIN_DIM_N001",string("x")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N002",string("y")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N003",string("axis_A")) ;
           SuperClassWriter::addAttribute("DOMAIN_DIM_N004",string("time_counter")) ;
  */
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Local Attributes IOIPSL \n");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeLocalAttributes_IOIPSL \
                  (int ibegin, int ni, int jbegin, int nj, int ni_glo, int nj_glo, int rank, int size)", << msg);
         }
      }
      //---------------------------------------------------------------

      void CNc4DataOutput:: writeFileAttributes(const StdString & name,
                                                const StdString & description,
                                                const StdString & conventions,
                                                const StdString & production,
                                                const StdString & timeStamp)
      {
         try
         {
           SuperClassWriter::addAttribute("name"       , name);
           SuperClassWriter::addAttribute("description", description);
           SuperClassWriter::addAttribute("title"      , description);
           SuperClassWriter::addAttribute("Conventions", conventions);
           SuperClassWriter::addAttribute("production" , production);
           SuperClassWriter::addAttribute("timeStamp"  , timeStamp);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing File Attributes \n ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput:: writeFileAttributes(const StdString & name, \
                                                const StdString & description, \
                                                const StdString & conventions, \
                                                const StdString & production, \
                                                const StdString & timeStamp)", << msg);
         }
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeMaskAttributes(const StdString & mask_name,
                                               int data_dim,
                                               int data_ni,
                                               int data_nj,
                                               int data_ibegin,
                                               int data_jbegin)
      {
         try
         {
           SuperClassWriter::addAttribute("data_dim"   , data_dim   , &mask_name);
           SuperClassWriter::addAttribute("data_ni"    , data_ni    , &mask_name);
           SuperClassWriter::addAttribute("data_nj"    , data_nj    , &mask_name);
           SuperClassWriter::addAttribute("data_ibegin", data_ibegin, &mask_name);
           SuperClassWriter::addAttribute("data_jbegin", data_jbegin, &mask_name);
         }
         catch (CNetCdfException& e)
         {
           StdString msg("On writing Mask Attributes \n ");
           msg.append("In the context : ");
           CContext* context = CContext::getCurrent() ;
           msg.append(context->getId()); msg.append("\n");
           msg.append(e.what());
           ERROR("CNc4DataOutput::writeMaskAttributes(const StdString & mask_name, \
                                               int data_dim, \
                                               int data_ni, \
                                               int data_nj, \
                                               int data_ibegin, \
                                               int data_jbegin)", << msg);
         }
      }

      ///--------------------------------------------------------------

} // namespace xios
