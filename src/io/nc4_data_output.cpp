#include "nc4_data_output.hpp"

#include "attribute_template.hpp"
#include "group_template.hpp"

#include "file.hpp"
#include "calendar.hpp"
#include "context.hpp"
#include "context_server.hpp"
#include "netCdfException.hpp"
#include "exception.hpp"
#include "timer.hpp"
#include "uuid.hpp"

#include <limits.h>
#include <cfloat>
#define X_FLOAT_MAX     FLT_MAX
#define X_FLOAT_MIN     FLT_MIN
#define X_SHORT_MAX     SHRT_MAX
#define X_SHORT_MIN     SHRT_MIN

namespace xios
{
      extern CLogType logProfile ;
      /// ////////////////////// Dfinitions ////////////////////// ///
      CNc4DataOutput::CNc4DataOutput
         (CFile* file, const StdString & filename, bool exist)
            : SuperClass()
            , SuperClassWriter(filename, exist)
            , filename(filename)
            , file(file),hasTimeInstant(false),hasTimeCentered(false), timeCounterType(none), relAxis_(), relDomains_()
      {
        SuperClass::type = MULTI_FILE;
        compressionLevel= file->compression_level.isEmpty() ? 0 :file->compression_level ;
      }

      CNc4DataOutput::CNc4DataOutput
         (CFile* file, const StdString & filename, bool exist, bool useClassicFormat, bool useCFConvention,
          MPI_Comm comm_file, bool multifile, bool isCollective, const StdString& timeCounterName)
            : SuperClass()
            , SuperClassWriter(filename, exist, useClassicFormat, useCFConvention, &comm_file, multifile, timeCounterName)
            , comm_file(comm_file)
            , filename(filename)
            , isCollective(isCollective)
            , file(file),hasTimeInstant(false),hasTimeCentered(false), timeCounterType(none), relAxis_(), relDomains_()
      {
        SuperClass::type = (multifile) ? MULTI_FILE : ONE_FILE;
        if (file==NULL) compressionLevel = 0 ;
        else compressionLevel= file->compression_level.isEmpty() ? 0 :file->compression_level ;
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
      TRY
      {
        StdString lonName,latName ;

        // Check that the name associated to the current element is not in conflict with an existing element (due to CGrid::duplicateSentGrid)
        if (!domain->lonvalue.isEmpty() )
        {
          // The hash of the element will be associated to the default element name (= map key), and to the name really written
          int globalHash = domain->computeAttributesHash( comm_file ); // Need a MPI_Comm to distribute without redundancy some attributs (value)
        
          StdString defaultNameKey = domain->getDomainOutputName();
          if ( !relDomains_.count ( defaultNameKey ) )
          {
            // if defaultNameKey not in the map, write the element such as it is defined
            relDomains_.insert( make_pair( defaultNameKey, make_pair(globalHash, domain) ) );
          }
          else // look if a hash associated this key is equal
          {
            bool elementIsInMap(false);
            auto defaultNameKeyElements = relDomains_.equal_range( defaultNameKey );
            for (auto it = defaultNameKeyElements.first; it != defaultNameKeyElements.second; it++)
            {
              if ( it->second.first == globalHash )
              {
                // if yes, associate the same ids to current element
                domain->renameAttributesBeforeWriting( it->second.second );
                elementIsInMap = true;
              }
            }
            // if no : inheritance has been excessive, define new names and store it (could be used by another grid)
            if (!elementIsInMap)  // ! in MAP
            {
              domain->renameAttributesBeforeWriting();
              relDomains_.insert( make_pair( defaultNameKey, make_pair(globalHash, domain) ) ) ;         
            }
          }
        }

        if (domain->type == CDomain::type_attr::unstructured)
        {
          if (SuperClassWriter::useCFConvention)
            writeUnstructuredDomain(domain) ;
          else
            writeUnstructuredDomainUgrid(domain) ;
          return ;
        }

         CContext* context = CContext::getCurrent() ;
         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return;

         
         std::vector<StdString> dim0, dim1;
         StdString domid = domain->getDomainOutputName();
         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;
         if (isWrittenDomain(domid)) return ;
         else setWrittenDomain(domid);
        
         int nvertex = (domain->nvertex.isEmpty()) ? 0 : domain->nvertex;


        StdString dimXid, dimYid ;

        nc_type typePrec ;
        if (domain->prec.isEmpty()) typePrec =  NC_FLOAT ;
        else if (domain->prec==4)  typePrec =  NC_FLOAT ;
        else if (domain->prec==8)   typePrec =  NC_DOUBLE ;
         
         bool isRegularDomain = (domain->type == CDomain::type_attr::rectilinear);
         switch (domain->type)
         {
           case CDomain::type_attr::curvilinear :

             if (domain->lon_name.isEmpty()) lonName = "nav_lon";
             else lonName = domain->lon_name;

             if (domain->lat_name.isEmpty()) latName = "nav_lat";
             else latName = domain->lat_name;

             if (domain->dim_i_name.isEmpty()) dimXid=StdString("x").append(appendDomid);
             else dimXid=domain->dim_i_name.getValue() + appendDomid;

             if (domain->dim_j_name.isEmpty()) dimYid=StdString("y").append(appendDomid);
             else dimYid=domain->dim_j_name.getValue() + appendDomid;

             break ;

           case CDomain::type_attr::rectilinear :

             if (domain->lon_name.isEmpty())
             {
               if (domain->dim_i_name.isEmpty())
                   lonName = "lon";
               else
                 lonName = domain->dim_i_name.getValue();
             }
             else lonName = domain->lon_name;

             if (domain->lat_name.isEmpty())
             {
               if (domain->dim_j_name.isEmpty())
                 latName = "lat";
               else
                 latName = domain->dim_j_name.getValue();
             }
             else latName = domain->lat_name;
             
             if (domain->dim_i_name.isEmpty()) dimXid = lonName+appendDomid;
             else dimXid = domain->dim_i_name.getValue()+appendDomid;

             if (domain->dim_j_name.isEmpty()) dimYid = latName+appendDomid;
             else dimYid = domain->dim_j_name.getValue()+appendDomid;
             break;
         }

         StdString dimVertId = StdString("nvertex").append(appendDomid);

         string lonid,latid,bounds_lonid,bounds_latid ;
         string areaId = "area" + appendDomid;

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 switch (domain->type)
                 {
                   case CDomain::type_attr::curvilinear :
                     dim0.push_back(dimYid); dim0.push_back(dimXid);
                     lonid = lonName+appendDomid;
                     latid = latName+appendDomid;
                     break ;
                   case CDomain::type_attr::rectilinear :
                     lonid = lonName+appendDomid;
                     latid = latName+appendDomid;
                     dim0.push_back(dimYid);
                     dim1.push_back(dimXid);
                     break;
                 }
                 if (!domain->bounds_lon_name.isEmpty()) bounds_lonid = domain->bounds_lon_name;
                 else bounds_lonid = "bounds_"+lonName+appendDomid;
                 if (!domain->bounds_lat_name.isEmpty()) bounds_latid = domain->bounds_lat_name;
                 else bounds_latid = "bounds_"+latName+appendDomid;

                 SuperClassWriter::addDimension(dimXid, domain->niValue_);
                 SuperClassWriter::addDimension(dimYid, domain->njValue_);

                 if (domain->hasBounds)
                   SuperClassWriter::addDimension(dimVertId, domain->nvertex);

                 if (context->intraCommSize_ > 1)
                 {
                   this->writeLocalAttributes(domain->ibeginValue_,
                                              domain->niValue_,
                                              domain->jbeginValue_,
                                              domain->njValue_,
                                              appendDomid);

                   if (singleDomain)
                    this->writeLocalAttributes_IOIPSL(dimXid, dimYid,
                                                      domain->ibeginValue_,
                                                      domain->niValue_,
                                                      domain->jbeginValue_,
                                                      domain->njValue_,
                                                      domain->ni_glo,domain->nj_glo,
                                                      context->intraCommRank_,context->intraCommSize_);
                 }

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       SuperClassWriter::addVariable(latid, typePrec, dim0, compressionLevel);
                       SuperClassWriter::addVariable(lonid, typePrec, dim0, compressionLevel);
                       break ;
                      case CDomain::type_attr::rectilinear :
                        SuperClassWriter::addVariable(latid, typePrec, dim0, compressionLevel);
                        SuperClassWriter::addVariable(lonid, typePrec, dim1, compressionLevel);
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
                     SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0, compressionLevel);
                     SuperClassWriter::addVariable(bounds_latid, typePrec, dim0, compressionLevel);
                   }
                 }

                 dim0.clear();
                 dim0.push_back(dimYid);
                 dim0.push_back(dimXid);

                 if (domain->hasArea)
                 {
                   SuperClassWriter::addVariable(areaId, typePrec, dim0, compressionLevel);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :                       
                       SuperClassWriter::writeData(domain->latvalue, latid, isCollective, 0);
                       SuperClassWriter::writeData(domain->lonvalue, lonid, isCollective, 0);
                       break;
                     case CDomain::type_attr::rectilinear :
                       //CArray<double,1> lat = domain->latvalue(Range(fromStart,toEnd,domain->ni)) ;
                       CArray<double,1> lat;
                       lat.resize( domain->njValue_);
                       for (int j=0;j<domain->njValue_;j++) lat(j) = domain->latvalue(j*domain->niValue_);
                       SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0);
                       //CArray<double,1> lon = domain->lonvalue(Range(0,domain->ni-1)) ;
                       CArray<double,1> lon = domain->lonvalue(Range(0,domain->niValue_-1));
                       SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0);
                       break;
                   }

                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lonvalue, bounds_lonid, isCollective, 0);
                     SuperClassWriter::writeData(domain->bounds_latvalue, bounds_latid, isCollective, 0);
                   }
                 }

                 if (domain->hasArea)
                 {
                   SuperClassWriter::writeData(domain->areavalue, areaId, isCollective, 0);                   
                 }

                 SuperClassWriter::definition_start();

                 break;
              }
              case (ONE_FILE) :
              {
                SuperClassWriter::addDimension(dimXid, domain->ni_glo);
                SuperClassWriter::addDimension(dimYid, domain->nj_glo);

                 if (domain->hasBounds)
                   SuperClassWriter::addDimension(dimVertId, domain->nvertex);

                 if (domain->hasLonLat)
                 {
                   switch (domain->type)
                   {
                     case CDomain::type_attr::curvilinear :
                       dim0.push_back(dimYid); dim0.push_back(dimXid);
                       lonid = lonName+appendDomid;
                       latid = latName+appendDomid;
                       SuperClassWriter::addVariable(latid, typePrec, dim0, compressionLevel);
                       SuperClassWriter::addVariable(lonid, typePrec, dim0, compressionLevel);
                       break;

                     case CDomain::type_attr::rectilinear :
                       dim0.push_back(dimYid);
                       dim1.push_back(dimXid);
                       lonid = lonName+appendDomid;
                       latid = latName+appendDomid;
                       SuperClassWriter::addVariable(latid, typePrec, dim0, compressionLevel);
                       SuperClassWriter::addVariable(lonid, typePrec, dim1, compressionLevel);
                       break;
                   }
                   if (!domain->bounds_lon_name.isEmpty()) bounds_lonid = domain->bounds_lon_name;
                   else bounds_lonid = "bounds_"+lonName+appendDomid;
                   if (!domain->bounds_lat_name.isEmpty()) bounds_latid = domain->bounds_lat_name;
                   else bounds_latid = "bounds_"+latName+appendDomid;

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
                     SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0, compressionLevel);
                     SuperClassWriter::addVariable(bounds_latid, typePrec, dim0, compressionLevel);
                   }
                 }

                 if (domain->hasArea)
                 {
                   dim0.clear();
                   dim0.push_back(dimYid); dim0.push_back(dimXid);
                   SuperClassWriter::addVariable(areaId, typePrec, dim0, compressionLevel);
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
                     start[1]=domain->ibeginValue_;
                     start[0]=domain->jbeginValue_;
                     count[1]=domain->niValue_ ; count[0]=domain->njValue_ ;

                     if (domain->hasLonLat)
                     {
                       SuperClassWriter::writeData(domain->latvalue, latid, isCollective, 0,&start,&count);
                       SuperClassWriter::writeData(domain->lonvalue, lonid, isCollective, 0,&start,&count);
                     }
                     break;
                   }
                   case CDomain::type_attr::rectilinear :
                   {
                     if (domain->hasLonLat)
                     {
                       std::vector<StdSize> start(1) ;
                       std::vector<StdSize> count(1) ;
                       
                       start[0]=domain->jbeginValue_;
                       count[0]=domain->njValue_;
                       CArray<double,1> lat;
                       lat.resize( domain->njValue_);
                       for (int j=0;j<domain->njValue_;j++) lat(j) = domain->latvalue(j*domain->niValue_);
                       SuperClassWriter::writeData(CArray<double,1>(lat.copy()), latid, isCollective, 0,&start,&count);

                       start[0]=domain->ibeginValue_;
                       count[0]=domain->niValue_;
                       CArray<double,1> lon;
                       lon.resize( domain->niValue_);
                       for (int i=0;i<domain->niValue_;i++) lon(i) = domain->lonvalue(i);
                       SuperClassWriter::writeData(CArray<double,1>(lon.copy()), lonid, isCollective, 0,&start,&count);
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
                     start[1] = domain->ibeginValue_;
                     start[0] = domain->jbeginValue_;
                     count[2] = domain->nvertex;
                     count[1] = domain->niValue_;
                     count[0] = domain->njValue_;
                   }
                 
                   SuperClassWriter::writeData(domain->bounds_lonvalue, bounds_lonid, isCollective, 0, &start, &count); // will probably not working for rectilinear
                   SuperClassWriter::writeData(domain->bounds_latvalue, bounds_latid, isCollective, 0, &start, &count);
                 }

                 if (domain->hasArea)
                 {
                   std::vector<StdSize> start(2);
                   std::vector<StdSize> count(2);

                   start[1] = domain->ibeginValue_;
                   start[0] = domain->jbeginValue_;
                   count[1] = domain->niValue_;
                   count[0] = domain->njValue_;
                   
                   SuperClassWriter::writeData(domain->areavalue, areaId, isCollective, 0, &start, &count);
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
      CATCH

    //--------------------------------------------------------------

    void CNc4DataOutput::writeUnstructuredDomainUgrid(CDomain* domain)
    {
      CContext* context = CContext::getCurrent() ;

      if (domain->IsWritten(this->filename)) return;

      StdString domid = domain->getDomainOutputName();

      // The first domain for the same mesh that will be written is that with the highest value of nvertex.
      // Thus the entire mesh connectivity will be generated at once.
      if (isWrittenDomain(domid)) return ;
      else setWrittenDomain(domid);

      domain->checkAttributes();
      if (domain->isEmpty())
        if (SuperClass::type==MULTI_FILE) return ;

     nc_type typePrec ;
     if (domain->prec.isEmpty()) typePrec =  NC_FLOAT ;
     else if (domain->prec==4)  typePrec =  NC_FLOAT ;
     else if (domain->prec==8)   typePrec =  NC_DOUBLE ;

      std::vector<StdString> dim0;
      StdString domainName = domain->name;

      StdString node_x = domainName + "_node_x";
      StdString node_y = domainName + "_node_y";

      StdString edge_x = domainName + "_edge_x";
      StdString edge_y = domainName + "_edge_y";
      StdString edge_nodes = domainName + "_edge_nodes";

      StdString face_x = domainName + "_face_x";
      StdString face_y = domainName + "_face_y";
      StdString face_nodes = domainName + "_face_nodes";
      StdString face_edges = domainName + "_face_edges";
      StdString edge_faces = domainName + "_edge_face_links";
      StdString face_faces = domainName + "_face_links";

      StdString dimNode = "n" + domainName + "_node";
      StdString dimEdge = "n" + domainName + "_edge";
      StdString dimFace = "n" + domainName + "_face";
      StdString dimVertex = "n" + domainName + "_vertex";
      StdString dimTwo = "Two";

      if (!SuperClassWriter::dimExist(dimTwo)) SuperClassWriter::addDimension(dimTwo, 2);
      dim0.clear();
      SuperClassWriter::addVariable(domainName, NC_INT, dim0, compressionLevel);
      SuperClassWriter::addAttribute("cf_role", StdString("mesh_topology"), &domainName);
      SuperClassWriter::addAttribute("long_name", StdString("Topology data of 2D unstructured mesh"), &domainName);
      SuperClassWriter::addAttribute("topology_dimension", 2, &domainName);
      SuperClassWriter::addAttribute("node_coordinates", node_x + " " + node_y, &domainName);

      try
      {
        switch (SuperClass::type)
        {
          case (ONE_FILE) :
          {
            // Adding nodes
            if (domain->nvertex == 1)
            {
              if (!SuperClassWriter::varExist(node_x) || !SuperClassWriter::varExist(node_y))
              {
                SuperClassWriter::addDimension(dimNode, domain->ni_glo);
                dim0.clear();
                dim0.push_back(dimNode);
                SuperClassWriter::addVariable(node_x, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &node_x);
                SuperClassWriter::addAttribute("long_name", StdString("Longitude of mesh nodes."), &node_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &node_x);
                SuperClassWriter::addVariable(node_y, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &node_y);
                SuperClassWriter::addAttribute("long_name", StdString("Latitude of mesh nodes."), &node_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &node_y);
              }
            } // domain->nvertex == 1

            // Adding edges and nodes, if nodes have not been defined previously
            if (domain->nvertex == 2)
            {
              if (!SuperClassWriter::varExist(node_x) || !SuperClassWriter::varExist(node_y))
              {
                SuperClassWriter::addDimension(dimNode, domain->mesh->nbNodesGlo);
                dim0.clear();
                dim0.push_back(dimNode);
                SuperClassWriter::addVariable(node_x, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &node_x);
                SuperClassWriter::addAttribute("long_name", StdString("Longitude of mesh nodes."), &node_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &node_x);
                SuperClassWriter::addVariable(node_y, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &node_y);
                SuperClassWriter::addAttribute("long_name", StdString("Latitude of mesh nodes."), &node_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &node_y);
              }
              SuperClassWriter::addAttribute("edge_node_connectivity", edge_nodes, &domainName);
              SuperClassWriter::addAttribute("edge_coordinates", edge_x + " " + edge_y, &domainName);
              SuperClassWriter::addDimension(dimEdge, domain->ni_glo);
              dim0.clear();
              dim0.push_back(dimEdge);
              SuperClassWriter::addVariable(edge_x, typePrec, dim0, compressionLevel);
              SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &edge_x);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic longitude of mesh edges."), &edge_x);
              SuperClassWriter::addAttribute("units", StdString("degrees_east"), &edge_x);
              SuperClassWriter::addVariable(edge_y, typePrec, dim0, compressionLevel);
              SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &edge_y);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic latitude of mesh edges."), &edge_y);
              SuperClassWriter::addAttribute("units", StdString("degrees_north"), &edge_y);
              dim0.clear();
              dim0.push_back(dimEdge);
              dim0.push_back(dimTwo);
              SuperClassWriter::addVariable(edge_nodes, NC_INT, dim0, compressionLevel);
              SuperClassWriter::addAttribute("cf_role", StdString("edge_node_connectivity"), &edge_nodes);
              SuperClassWriter::addAttribute("long_name", StdString("Maps every edge/link to two nodes that it connects."), &edge_nodes);
              SuperClassWriter::addAttribute("start_index", 0, &edge_nodes);
            } // domain->nvertex == 2

            // Adding faces, edges, and nodes, if edges and nodes have not been defined previously
            if (domain->nvertex > 2)
            {
              // Nodes
              if (!SuperClassWriter::varExist(node_x) || !SuperClassWriter::varExist(node_y))
              {
                SuperClassWriter::addDimension(dimNode, domain->mesh->nbNodesGlo);
                dim0.clear();
                dim0.push_back(dimNode);
                SuperClassWriter::addVariable(node_x, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &node_x);
                SuperClassWriter::addAttribute("long_name", StdString("Longitude of mesh nodes."), &node_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &node_x);
                SuperClassWriter::addVariable(node_y, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &node_y);
                SuperClassWriter::addAttribute("long_name", StdString("Latitude of mesh nodes."), &node_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &node_y);
              }
              if (!SuperClassWriter::varExist(edge_x) || !SuperClassWriter::varExist(edge_y))
              {
                SuperClassWriter::addAttribute("edge_coordinates", edge_x + " " + edge_y, &domainName);
                SuperClassWriter::addAttribute("edge_node_connectivity", edge_nodes, &domainName);
                SuperClassWriter::addAttribute("face_edge_connectivity", face_edges, &domainName);
                SuperClassWriter::addAttribute("edge_face_connectivity", edge_faces, &domainName);
                SuperClassWriter::addDimension(dimEdge, domain->mesh->nbEdgesGlo);
                dim0.clear();
                dim0.push_back(dimEdge);
                SuperClassWriter::addVariable(edge_x, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &edge_x);
                SuperClassWriter::addAttribute("long_name", StdString("Characteristic longitude of mesh edges."), &edge_x);
                SuperClassWriter::addAttribute("units", StdString("degrees_east"), &edge_x);
                SuperClassWriter::addVariable(edge_y, typePrec, dim0, compressionLevel);
                SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &edge_y);
                SuperClassWriter::addAttribute("long_name", StdString("Characteristic latitude of mesh edges."), &edge_y);
                SuperClassWriter::addAttribute("units", StdString("degrees_north"), &edge_y);
                dim0.clear();
                dim0.push_back(dimEdge);
                dim0.push_back(dimTwo);
                SuperClassWriter::addVariable(edge_nodes, NC_INT, dim0, compressionLevel);
                SuperClassWriter::addAttribute("cf_role", StdString("edge_node_connectivity"), &edge_nodes);
                SuperClassWriter::addAttribute("long_name", StdString("Maps every edge/link to two nodes that it connects."), &edge_nodes);
                SuperClassWriter::addAttribute("start_index", 0, &edge_nodes);
              }
              SuperClassWriter::addAttribute("face_face_connectivity", face_faces, &domainName);
              SuperClassWriter::addAttribute("face_coordinates", face_x + " " + face_y, &domainName);
              SuperClassWriter::addAttribute("face_node_connectivity", face_nodes, &domainName);
              SuperClassWriter::addDimension(dimFace, domain->ni_glo);
              SuperClassWriter::addDimension(dimVertex, domain->nvertex);
              dim0.clear();
              dim0.push_back(dimFace);
              SuperClassWriter::addVariable(face_x, typePrec, dim0, compressionLevel);
              SuperClassWriter::addAttribute("standard_name", StdString("longitude"), &face_x);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic longitude of mesh faces."), &face_x);
              SuperClassWriter::addAttribute("units", StdString("degrees_east"), &face_x);
              SuperClassWriter::addVariable(face_y, typePrec, dim0, compressionLevel);
              SuperClassWriter::addAttribute("standard_name", StdString("latitude"), &face_y);
              SuperClassWriter::addAttribute("long_name", StdString("Characteristic latitude of mesh faces."), &face_y);
              SuperClassWriter::addAttribute("units", StdString("degrees_north"), &face_y);
              dim0.clear();
              dim0.push_back(dimFace);
              dim0.push_back(dimVertex);
              SuperClassWriter::addVariable(face_nodes, NC_INT, dim0, compressionLevel);
              SuperClassWriter::addAttribute("cf_role", StdString("face_node_connectivity"), &face_nodes);
              SuperClassWriter::addAttribute("long_name", StdString("Maps every face to its corner nodes."), &face_nodes);
              SuperClassWriter::addAttribute("start_index", 0, &face_nodes);
              dim0.clear();
              dim0.push_back(dimFace);
              dim0.push_back(dimVertex);
              SuperClassWriter::addVariable(face_edges, NC_INT, dim0, compressionLevel);
              SuperClassWriter::addAttribute("cf_role", StdString("face_edge_connectivity"), &face_edges);
              SuperClassWriter::addAttribute("long_name", StdString("Maps every face to its edges."), &face_edges);
              SuperClassWriter::addAttribute("start_index", 0, &face_edges);
              SuperClassWriter::addAttribute("_FillValue", 999999, &face_edges);
              dim0.clear();
              dim0.push_back(dimEdge);
              dim0.push_back(dimTwo);
              SuperClassWriter::addVariable(edge_faces, NC_INT, dim0, compressionLevel);
              SuperClassWriter::addAttribute("cf_role", StdString("edge_face_connectivity"), &edge_faces);
              SuperClassWriter::addAttribute("long_name", StdString("neighbor faces for edges"), &edge_faces);
              SuperClassWriter::addAttribute("start_index", 0, &edge_faces);
              SuperClassWriter::addAttribute("_FillValue", -999, &edge_faces);
              SuperClassWriter::addAttribute("comment", StdString("missing neighbor faces are indicated using _FillValue"), &edge_faces);
              dim0.clear();
              dim0.push_back(dimFace);
              dim0.push_back(dimVertex);
              SuperClassWriter::addVariable(face_faces, NC_INT, dim0, compressionLevel);
              SuperClassWriter::addAttribute("cf_role", StdString("face_face_connectivity"), &face_faces);
              SuperClassWriter::addAttribute("long_name", StdString("Indicates which other faces neighbor each face"), &face_faces);
              SuperClassWriter::addAttribute("start_index", 0, &face_faces);
              SuperClassWriter::addAttribute("_FillValue", 999999, &face_faces);
              SuperClassWriter::addAttribute("flag_values", -1, &face_faces);
              SuperClassWriter::addAttribute("flag_meanings", StdString("out_of_mesh"), &face_faces);
            } // domain->nvertex > 2

            SuperClassWriter::definition_end();

            std::vector<StdSize> startEdges(1) ;
            std::vector<StdSize> countEdges(1) ;
            std::vector<StdSize> startNodes(1) ;
            std::vector<StdSize> countNodes(1) ;
            std::vector<StdSize> startFaces(1) ;
            std::vector<StdSize> countFaces(1) ;
            std::vector<StdSize> startEdgeNodes(2) ;
            std::vector<StdSize> countEdgeNodes(2) ;
            std::vector<StdSize> startEdgeFaces(2) ;
            std::vector<StdSize> countEdgeFaces(2) ;
            std::vector<StdSize> startFaceConctv(2) ;
            std::vector<StdSize> countFaceConctv(2) ;

            if (domain->nvertex == 1)
            {
              if (domain->isEmpty())
               {
                 startNodes[0]=0 ;
                 countNodes[0]=0 ;
               }
               else
               {
                 startNodes[0] = domain->ibeginValue_;
                 countNodes[0] = domain->niValue_ ;
               }

              SuperClassWriter::writeData(domain->mesh->node_lat, node_y, isCollective, 0, &startNodes, &countNodes);
              SuperClassWriter::writeData(domain->mesh->node_lon, node_x, isCollective, 0, &startNodes, &countNodes);
            }
            else if (domain->nvertex == 2)
            {
              if (domain->isEmpty())
               {
                startEdges[0]=0 ;
                countEdges[0]=0 ;
                startNodes[0]=0 ;
                countNodes[0]=0 ;
                startEdgeNodes[0]=0;
                startEdgeNodes[1]=0;
                countEdgeNodes[0]=0;
                countEdgeNodes[1]=0;

               }
               else
               {
                 startEdges[0] = domain->ibeginValue_;
                 countEdges[0] = domain->niValue_;
                 startNodes[0] = domain->mesh->node_start;
                 countNodes[0] = domain->mesh->node_count;
                 if (countNodes[0]==0) startNodes[0]=0 ; // for netcdf error
                 startEdgeNodes[0] = domain->ibeginValue_;
                 startEdgeNodes[1] = 0;
                 countEdgeNodes[0] = domain->niValue_;
                 countEdgeNodes[1] = 2;
               }
              SuperClassWriter::writeData(domain->mesh->node_lat, node_y, isCollective, 0, &startNodes, &countNodes);
              SuperClassWriter::writeData(domain->mesh->node_lon, node_x, isCollective, 0, &startNodes, &countNodes);
              SuperClassWriter::writeData(domain->mesh->edge_lat, edge_y, isCollective, 0, &startEdges, &countEdges);
              SuperClassWriter::writeData(domain->mesh->edge_lon, edge_x, isCollective, 0, &startEdges, &countEdges);
              SuperClassWriter::writeData(domain->mesh->edge_nodes, edge_nodes, isCollective, 0, &startEdgeNodes, &countEdgeNodes);
            }
            else
            {
              if (domain->isEmpty())
               {
                 startFaces[0] = 0 ;
                 countFaces[0] = 0 ;
                 startNodes[0] = 0;
                 countNodes[0] = 0;
                 startEdges[0] = 0;
                 countEdges[0] = 0;
                 startEdgeFaces[0] = 0;
                 startEdgeFaces[1] = 0;
                 countEdgeFaces[0] = 0;
                 countEdgeFaces[1] = 0;
                 startFaceConctv[0] = 0;
                 startFaceConctv[1] = 0;
                 countFaceConctv[0] = 0;
                 countFaceConctv[1] = 0;
               }
               else
               {
                 startFaces[0] = domain->ibeginValue_;
                 countFaces[0] = domain->niValue_ ;
                 startNodes[0] = domain->mesh->node_start;
                 countNodes[0] = domain->mesh->node_count;
                 if (countNodes[0]==0) startNodes[0]=0;

                 startEdges[0] = domain->mesh->edge_start;
                 countEdges[0] = domain->mesh->edge_count;
                 if (countEdges[0]==0) startEdges[0]=0 ; // for netcdf error

                 startEdgeNodes[0] = domain->mesh->edge_start;
                 countEdgeNodes[0] = domain->mesh->edge_count;
                 if (countEdgeNodes[0]==0) startEdgeNodes[0]=0; // for netcdf error
                 startEdgeNodes[1] = 0;
                 countEdgeNodes[1]= 2;
                 
                 startEdgeFaces[0] = domain->mesh->edge_start;
                 countEdgeFaces[0] = domain->mesh->edge_count;
                 if (countEdgeFaces[0]==0) startEdgeFaces[0]=0 ; // for netcdf error
                 startEdgeFaces[1]= 0;
                 countEdgeFaces[1]= 2;
                 
                 startFaceConctv[0] = domain->ibeginValue_;
                 countFaceConctv[0] = domain->niValue_;
                 startFaceConctv[1] = 0;
                 countFaceConctv[1] = domain->nvertex;
               }
              SuperClassWriter::writeData(domain->mesh->node_lat, node_y, isCollective, 0, &startNodes, &countNodes);
              SuperClassWriter::writeData(domain->mesh->node_lon, node_x, isCollective, 0, &startNodes, &countNodes);
              SuperClassWriter::writeData(domain->mesh->edge_lat, edge_y, isCollective, 0, &startEdges, &countEdges);
              SuperClassWriter::writeData(domain->mesh->edge_lon, edge_x, isCollective, 0, &startEdges, &countEdges);
              SuperClassWriter::writeData(domain->mesh->edge_nodes, edge_nodes, isCollective, 0, &startEdgeNodes, &countEdgeNodes);
              SuperClassWriter::writeData(domain->mesh->face_lat, face_y, isCollective, 0, &startFaces, &countFaces);
              SuperClassWriter::writeData(domain->mesh->face_lon, face_x, isCollective, 0, &startFaces, &countFaces);
              SuperClassWriter::writeData(domain->mesh->face_nodes, face_nodes, isCollective, 0, &startFaceConctv, &countFaceConctv);
              SuperClassWriter::writeData(domain->mesh->face_edges, face_edges, isCollective, 0, &startFaceConctv, &countFaceConctv);
              SuperClassWriter::writeData(domain->mesh->edge_faces, edge_faces, isCollective, 0, &startEdgeFaces, &countEdgeFaces);
              SuperClassWriter::writeData(domain->mesh->face_faces, face_faces, isCollective, 0, &startFaceConctv, &countFaceConctv);
            }
            SuperClassWriter::definition_start();

            break;
          } // ONE_FILE

          case (MULTI_FILE) :
          {
            ERROR("CNc4DataOutput::writeDomain(domain)",
            << "[ type = multiple_file ]"
            << " is not yet implemented for UGRID files !");
            break;
          }

          default :
          ERROR("CNc4DataOutput::writeDomain(domain)",
          << "[ type = " << SuperClass::type << "]"
          << " not implemented yet !");
          } // switch
        } // try

        catch (CNetCdfException& e)
        {
          StdString msg("On writing the domain : ");
          msg.append(domid); msg.append("\n");
          msg.append("In the context : ");
          msg.append(context->getId()); msg.append("\n");
          msg.append(e.what());
          ERROR("CNc4DataOutput::writeUnstructuredDomainUgrid(CDomain* domain)", << msg);
        }

  domain->addRelFile(this->filename);
  }

    //--------------------------------------------------------------

    void CNc4DataOutput::writeUnstructuredDomain(CDomain* domain)
      {
         CContext* context = CContext::getCurrent() ;

         if (domain->IsWritten(this->filename)) return;
         domain->checkAttributes();

         if (domain->isEmpty())
           if (SuperClass::type==MULTI_FILE) return ;

         std::vector<StdString> dim0, dim1;
         StdString domid = domain->getDomainOutputName();
         if (isWrittenDomain(domid)) return ;
         else setWrittenDomain(domid);

         StdString appendDomid  = (singleDomain) ? "" : "_"+domid ;

         StdString lonName,latName, cellName ;
         if (domain->lon_name.isEmpty()) lonName = "lon";
         else lonName = domain->lon_name;

         if (domain->lat_name.isEmpty()) latName = "lat";
         else latName = domain->lat_name;

         if (!domain->dim_i_name.isEmpty()) cellName=domain->dim_i_name;
         else cellName="cell";
         StdString dimXid = cellName+appendDomid;
         StdString dimVertId;
         if (domain->nvertex_name.isEmpty()) dimVertId = "nvertex"+appendDomid; 
         else dimVertId = domain->nvertex_name;

         string lonid,latid,bounds_lonid,bounds_latid ;
         string areaId = "area" + appendDomid;

         nc_type typePrec ;
         if (domain->prec.isEmpty()) typePrec =  NC_FLOAT ;
         else if (domain->prec==4)  typePrec =  NC_FLOAT ;
         else if (domain->prec==8)   typePrec =  NC_DOUBLE ;

         int nvertex = (domain->nvertex.isEmpty()) ? 0 : domain->nvertex;

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->ni);

                 lonid = lonName+appendDomid;
                 latid = latName+appendDomid;
                 if (!domain->bounds_lon_name.isEmpty()) bounds_lonid = domain->bounds_lon_name;
                 else bounds_lonid = "bounds_"+lonName+appendDomid;
                 if (!domain->bounds_lat_name.isEmpty()) bounds_latid = domain->bounds_lat_name;
                 else bounds_latid = "bounds_"+latName+appendDomid;

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::addVariable(latid, typePrec, dim0, compressionLevel);
                   SuperClassWriter::addVariable(lonid, typePrec, dim0, compressionLevel);
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
                   SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0, compressionLevel);
                   SuperClassWriter::addVariable(bounds_latid, typePrec, dim0, compressionLevel);
                 }

                 dim0.clear();
                 dim0.push_back(dimXid);
                 if (domain->hasArea)
                 {
                   SuperClassWriter::addVariable(areaId, typePrec, dim0, compressionLevel);
                   SuperClassWriter::addAttribute("standard_name", StdString("cell_area"), &areaId);
                   SuperClassWriter::addAttribute("units", StdString("m2"), &areaId);
                 }

                 SuperClassWriter::definition_end();

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::writeData(domain->latvalue, latid, isCollective, 0);
                   SuperClassWriter::writeData(domain->lonvalue, lonid, isCollective, 0);
                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lonvalue, bounds_lonid, isCollective, 0);
                     SuperClassWriter::writeData(domain->bounds_latvalue, bounds_latid, isCollective, 0);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->areavalue, areaId, isCollective, 0);

                 SuperClassWriter::definition_start();
                 break ;
              }

              case (ONE_FILE) :
              {
                 lonid = lonName+appendDomid;
                 latid = latName+appendDomid;
                 if (!domain->bounds_lon_name.isEmpty()) bounds_lonid = domain->bounds_lon_name;
                 else bounds_lonid = "bounds_"+lonName+appendDomid;
                 if (!domain->bounds_lat_name.isEmpty()) bounds_latid = domain->bounds_lat_name;
                 else bounds_latid = "bounds_"+latName+appendDomid;

                 dim0.push_back(dimXid);
                 SuperClassWriter::addDimension(dimXid, domain->ni_glo);
                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::addVariable(latid, typePrec, dim0, compressionLevel);
                   SuperClassWriter::addVariable(lonid, typePrec, dim0, compressionLevel);

                   this->writeAxisAttributes(lonid, "", "longitude", "Longitude", "degrees_east", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_lonid, &lonid);
                   this->writeAxisAttributes(latid, "", "latitude", "Latitude", "degrees_north", domid);
                   if (domain->hasBounds) SuperClassWriter::addAttribute("bounds",bounds_latid, &latid);
                   if (domain->hasBounds) SuperClassWriter::addDimension(dimVertId, nvertex);
                 }
                 dim0.clear();

                 if (domain->hasBounds)
                 {
                   dim0.push_back(dimXid);
                   dim0.push_back(dimVertId);
                   SuperClassWriter::addVariable(bounds_lonid, typePrec, dim0, compressionLevel);
                   SuperClassWriter::addVariable(bounds_latid, typePrec, dim0, compressionLevel);
                 }

                 if (domain->hasArea)
                 {
                   dim0.clear();
                   dim0.push_back(dimXid);
                   SuperClassWriter::addVariable(areaId, typePrec, dim0, compressionLevel);
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
                   countBounds[1]=nvertex ;
                   startBounds[0]=0 ;
                   countBounds[0]=0 ;
                 }
                 else
                 {
                   start[0]=domain->ibeginValue_;
                   count[0]=domain->niValue_;
                   startBounds[0]=domain->ibeginValue_;
                   startBounds[1]=0 ;
                   countBounds[0]=domain->niValue_;
                   countBounds[1]=nvertex ;
                 }

                 if (domain->hasLonLat)
                 {
                   SuperClassWriter::writeData(domain->latvalue, latid, isCollective, 0,&start,&count);
                   SuperClassWriter::writeData(domain->lonvalue, lonid, isCollective, 0,&start,&count);
                   if (domain->hasBounds)
                   {
                     SuperClassWriter::writeData(domain->bounds_lonvalue, bounds_lonid, isCollective, 0,&startBounds,&countBounds);
                     SuperClassWriter::writeData(domain->bounds_latvalue, bounds_latid, isCollective, 0,&startBounds,&countBounds);
                   }
                 }

                 if (domain->hasArea)
                   SuperClassWriter::writeData(domain->areavalue, areaId, isCollective, 0, &start, &count);

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

        // Check that the name associated to the current element is not in conflict with an existing element (due to CGrid::duplicateSentGrid)
        if (!axis->value.isEmpty() )
        {
          // The hash of the element will be associated to the default element name (= map key), and to the name really written
          int globalHash = axis->computeAttributesHash( comm_file ); // Need a MPI_Comm to distribute without redundancy some attributs (value)

          StdString defaultNameKey = axis->getAxisOutputName();
          if ( !relAxis_.count ( defaultNameKey ) )
          {
            // if defaultNameKey not in the map, write the element such as it is defined
            relAxis_.insert( make_pair( defaultNameKey, make_pair(globalHash, axis) ) );
          }
          else // look if a hash associated this key is equal
          {
            bool elementIsInMap(false);
            auto defaultNameKeyElements = relAxis_.equal_range( defaultNameKey );
            for (auto it = defaultNameKeyElements.first; it != defaultNameKeyElements.second; it++)
            {
              if ( it->second.first == globalHash )
              {
                // if yes, associate the same ids to current element
                axis->renameAttributesBeforeWriting( it->second.second );
                elementIsInMap = true;
              }
            }
             // if no : inheritance has been excessive, define new names and store it (could be used by another grid)
            if (!elementIsInMap)  // ! in MAP
            {
              axis->renameAttributesBeforeWriting();
              relAxis_.insert( make_pair( defaultNameKey, make_pair(globalHash, axis) ) ) ;// = axis->getId()          
            }
          }
        }

        axis->checkAttributes();

        int size  = (MULTI_FILE == SuperClass::type) ? axis->n.getValue()
                                                          : axis->n_glo.getValue();

        if ((0 == axis->n) && (MULTI_FILE == SuperClass::type)) return;

        std::vector<StdString> dims;
        StdString axisid = axis->getAxisOutputName();
        StdString axisDim, axisBoundsId;
        if (isWrittenAxis(axisid)) return ;
        else setWrittenAxis(axisid);

        nc_type typePrec ;
        if (axis->prec.isEmpty()) typePrec =  NC_FLOAT ;
        else if (axis->prec==4)   typePrec =  NC_FLOAT ;
        else if (axis->prec==8)   typePrec =  NC_DOUBLE ;
         
        if (!axis->label.isEmpty()) typePrec = NC_CHAR ;
        string strId="str_len" ;
        try
        {
          if (axis->dim_name.isEmpty()) axisDim = axisid;
          else axisDim=axis->dim_name.getValue();
          SuperClassWriter::addDimension(axisDim, size);
          dims.push_back(axisDim);

          if (!axis->label.isEmpty() && !SuperClassWriter::dimExist(strId)) SuperClassWriter::addDimension(strId, stringArrayLen);

          if (axis->hasValue() || !axis->label.isEmpty())
          {
            if (!axis->label.isEmpty()) dims.push_back(strId);

            SuperClassWriter::addVariable(axisid, typePrec, dims, compressionLevel);

            if (!axis->name.isEmpty())
              SuperClassWriter::addAttribute("name", axis->name.getValue(), &axisid);

            if (!axis->standard_name.isEmpty())
              SuperClassWriter::addAttribute("standard_name", axis->standard_name.getValue(), &axisid);

            if (!axis->long_name.isEmpty())
              SuperClassWriter::addAttribute("long_name", axis->long_name.getValue(), &axisid);

            if (!axis->unit.isEmpty())
              SuperClassWriter::addAttribute("units", axis->unit.getValue(), &axisid);

            if (!axis->axis_type.isEmpty())
            {
              switch(axis->axis_type)
              {
              case CAxis::axis_type_attr::X :
                SuperClassWriter::addAttribute("axis", string("X"), &axisid);
                break;
              case CAxis::axis_type_attr::Y :
                SuperClassWriter::addAttribute("axis", string("Y"), &axisid);
                break;
              case CAxis::axis_type_attr::Z :
                SuperClassWriter::addAttribute("axis", string("Z"), &axisid);
                break;
              case CAxis::axis_type_attr::T :
                SuperClassWriter::addAttribute("axis", string("T"), &axisid);
                break;
              }
            }

            if (!axis->positive.isEmpty())
            {
              SuperClassWriter::addAttribute("positive",
                                             (axis->positive == CAxis::positive_attr::up) ? string("up") : string("down"),
                                             &axisid);
            }

            if (!axis->formula.isEmpty())
              SuperClassWriter::addAttribute("formula", axis->formula.getValue(), &axisid);

            if (!axis->formula_term.isEmpty())
              SuperClassWriter::addAttribute("formula_terms", axis->formula_term.getValue(), &axisid);
              
            axisBoundsId = (axis->bounds_name.isEmpty()) ? axisid + "_bounds" : axis->bounds_name;
            if (!axis->bounds.isEmpty() && axis->label.isEmpty())
            {
              dims.push_back("axis_nbounds");
              SuperClassWriter::addVariable(axisBoundsId, typePrec, dims, compressionLevel);
              SuperClassWriter::addAttribute("bounds", axisBoundsId, &axisid);

              if (!axis->standard_name.isEmpty())
                SuperClassWriter::addAttribute("standard_name", axis->standard_name.getValue(), &axisBoundsId);

              if (!axis->unit.isEmpty())
                SuperClassWriter::addAttribute("units", axis->unit.getValue(), &axisBoundsId);

              if (!axis->formula_bounds.isEmpty())
                SuperClassWriter::addAttribute("formula", axis->formula_bounds.getValue(), &axisBoundsId);

              if (!axis->formula_term_bounds.isEmpty())
                SuperClassWriter::addAttribute("formula_terms", axis->formula_term_bounds.getValue(), &axisBoundsId);
            }
          }

          SuperClassWriter::definition_end();
          
          switch (SuperClass::type)
          {
            case MULTI_FILE:
            {
              if (axis->label.isEmpty())
              {
                if (!axis->value.isEmpty())
                  SuperClassWriter::writeData(axis->value, axisid, isCollective, 0);

                if (!axis->bounds.isEmpty())
                  SuperClassWriter::writeData(axis->bounds, axisBoundsId, isCollective, 0);
              }
              else
                SuperClassWriter::writeData(axis->label, axisid, isCollective, 0);

              SuperClassWriter::definition_start();
              break;
            }
            case ONE_FILE:
            {
              std::vector<StdSize> start(1), startBounds(2) ;
              std::vector<StdSize> count(1), countBounds(2) ;
              start[0] = startBounds[0] = axis->begin;
              count[0] = countBounds[0] = axis->n;
              startBounds[1] = 0;
              countBounds[1] = 2;

              if (axis->label.isEmpty())
              {
                if (!axis->value.isEmpty())
                  SuperClassWriter::writeData(axis->value, axisid, isCollective, 0, &start, &count);

                if (!axis->bounds.isEmpty())
                  SuperClassWriter::writeData(axis->bounds, axisBoundsId, isCollective, 0, &startBounds, &countBounds);
              }
              else
              {
                std::vector<StdSize> startLabel(2), countLabel(2);
                startLabel[0] = start[0]; startLabel[1] = 0;
                countLabel[0] = count[0]; countLabel[1] = stringArrayLen;
                SuperClassWriter::writeData(axis->label, axisid, isCollective, 0, &startLabel, &countLabel);
              }

              SuperClassWriter::definition_start();

              break;
            }
            default :
              ERROR("CNc4DataOutput::writeAxis_(CAxis* axis)",
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

      void CNc4DataOutput::writeScalar_(CScalar* scalar)
      {
        if (scalar->IsWritten(this->filename)) return;
        scalar->checkAttributes();
        int scalarSize = 1;

        StdString scalaId = scalar->getScalarOutputName();
        StdString boundsId;
        if (isWrittenAxis(scalaId)) return ;
        else setWrittenAxis(scalaId);

        nc_type typePrec ;
        if (scalar->prec.isEmpty()) typePrec =  NC_FLOAT ;
        else if (scalar->prec==4)  typePrec =  NC_FLOAT ;
        else if (scalar->prec==8)   typePrec =  NC_DOUBLE ;

        if (!scalar->label.isEmpty()) typePrec = NC_CHAR ;
        string strId="str_len" ;

        try
        {
          if (!scalar->label.isEmpty() && !SuperClassWriter::dimExist(strId)) SuperClassWriter::addDimension(strId, stringArrayLen);

          if (!scalar->value.isEmpty() || !scalar->label.isEmpty())
          {
            std::vector<StdString> dims;
            StdString scalarDim = scalaId;

            if (!scalar->label.isEmpty()) dims.push_back(strId);

            SuperClassWriter::addVariable(scalaId, typePrec, dims);

            if (!scalar->name.isEmpty())
              SuperClassWriter::addAttribute("name", scalar->name.getValue(), &scalaId);

            if (!scalar->standard_name.isEmpty())
              SuperClassWriter::addAttribute("standard_name", scalar->standard_name.getValue(), &scalaId);

            if (!scalar->long_name.isEmpty())
              SuperClassWriter::addAttribute("long_name", scalar->long_name.getValue(), &scalaId);

            if (!scalar->unit.isEmpty())
              SuperClassWriter::addAttribute("units", scalar->unit.getValue(), &scalaId);

            if (!scalar->axis_type.isEmpty())
            {
              switch(scalar->axis_type)
              {
              case CScalar::axis_type_attr::X :
                SuperClassWriter::addAttribute("axis", string("X"), &scalaId);
                break;
              case CScalar::axis_type_attr::Y :
                SuperClassWriter::addAttribute("axis", string("Y"), &scalaId);
                break;
              case CScalar::axis_type_attr::Z :
                SuperClassWriter::addAttribute("axis", string("Z"), &scalaId);
                break;
              case CScalar::axis_type_attr::T :
                SuperClassWriter::addAttribute("axis", string("T"), &scalaId);
                break;
              }
            }

            if (!scalar->positive.isEmpty())
            {
              SuperClassWriter::addAttribute("positive",
                                             (scalar->positive == CScalar::positive_attr::up) ? string("up") : string("down"),
                                             &scalaId);
            }

            if (!scalar->bounds.isEmpty() && scalar->label.isEmpty())
            {
              dims.clear();
              dims.push_back("axis_nbounds");
              boundsId = (scalar->bounds_name.isEmpty()) ? (scalaId + "_bounds") : scalar->bounds_name.getValue();
              SuperClassWriter::addVariable(boundsId, typePrec, dims);
              SuperClassWriter::addAttribute("bounds", boundsId, &scalaId);
            }

            SuperClassWriter::definition_end();

            switch (SuperClass::type)
            {
              case MULTI_FILE:
              {
                CArray<double,1> scalarValue(scalarSize);
                CArray<string,1> scalarLabel(scalarSize);
                CArray<double,1> scalarBounds(scalarSize*2);

                if (!scalar->value.isEmpty() && scalar->label.isEmpty())
                {
                  scalarValue(0) = scalar->value;
                  SuperClassWriter::writeData(scalarValue, scalaId, isCollective, 0);
                }

                if (!scalar->bounds.isEmpty() && scalar->label.isEmpty())
                {
                  scalarBounds(0) = scalar->bounds(0);
                  scalarBounds(1) = scalar->bounds(1);
                  SuperClassWriter::writeData(scalarBounds, boundsId, isCollective, 0);
                }

                if (!scalar->label.isEmpty())
                {
                  scalarLabel(0) = scalar->label;
                  SuperClassWriter::writeData(scalarLabel, scalaId, isCollective, 0);
                }

                SuperClassWriter::definition_start();

                break;
              }
              case ONE_FILE:
              {
                CArray<double,1> scalarValue(scalarSize);
                CArray<string,1> scalarLabel(scalarSize);
                CArray<double,1> scalarBounds(scalarSize*2);

                std::vector<StdSize> start(1);
                std::vector<StdSize> count(1);
                start[0] = 0;
                count[0] = 1;
                if (!scalar->value.isEmpty() && scalar->label.isEmpty())
                {
                  scalarValue(0) = scalar->value;
                  SuperClassWriter::writeData(scalarValue, scalaId, isCollective, 0, &start, &count);
                }
                if (!scalar->bounds.isEmpty() && scalar->label.isEmpty())
                {
                  scalarBounds(0) = scalar->bounds(0);
                  scalarBounds(1) = scalar->bounds(1);
                  count[0] = 2;
                  SuperClassWriter::writeData(scalarBounds, boundsId, isCollective, 0, &start, &count);
                }
                if (!scalar->label.isEmpty())
                {
                  scalarLabel(0) = scalar->label;
                  count[0] = stringArrayLen;
                  SuperClassWriter::writeData(scalarLabel, scalaId, isCollective, 0, &start, &count);
                }

                SuperClassWriter::definition_start();

                break;
              }
              default :
                ERROR("CNc4DataOutput::writeAxis_(CAxis* scalar)",
                      << "[ type = " << SuperClass::type << "]"
                      << " not implemented yet !");
            }
          }
        }
        catch (CNetCdfException& e)
        {
          StdString msg("On writing the scalar : ");
          msg.append(scalaId); msg.append("\n");
          msg.append("In the context : ");
          CContext* context = CContext::getCurrent() ;
          msg.append(context->getId()); msg.append("\n");
          msg.append(e.what());
          ERROR("CNc4DataOutput::writeScalar_(CScalar* scalar)", << msg);
        }
        scalar->addRelFile(this->filename);
     }

     //--------------------------------------------------------------

     void CNc4DataOutput::writeGridCompressed_(CGrid* grid)
     {
        if (grid->isScalarGrid() || grid->isWrittenCompressed(this->filename)) return;
        
        // NOTA : The cuurent algorithm to write compress elements of the grid
        //        will work pretting well when on server side you dont't get
        //        partial overlap on elements between differents participating process
        //        So the element must be totally distributed or non distributed
        //        If an element is partially overlaping betwwen process then the
        //        total compressed part will apear artificially greater than expected
        //        For the current implementation of writer which is decomposed only on
        //        one element, it will work as expected, but for future, it must be 
        //        reconsidered again. 
        try
        {
          CArray<int,1> axisDomainOrder = grid->axis_domain_order;
          std::vector<StdString> domainList = grid->getDomainList();
          std::vector<StdString> axisList   = grid->getAxisList();
          std::vector<StdString> scalarList = grid->getScalarList();
          int numElement = axisDomainOrder.numElements(), idxDomain = 0, idxAxis = 0, idxScalar = 0;
          int commRank ;
          MPI_Comm_rank(comm_file,&commRank) ;

          std::vector<StdString> dims;

          for (int i = 0; i < numElement; ++i)
          {
            StdString varId, compress;
            CArray<size_t, 1> indexes;
            bool isDistributed;
            size_t nbIndexes, totalNbIndexes, offset;
            size_t firstGlobalIndex;
            
            if (2 == axisDomainOrder(i))
            {
              CDomain* domain = CDomain::get(domainList[idxDomain]);
              StdString domId = domain->getDomainOutputName();

              if (!domain->isCompressible()
                  || domain->type == CDomain::type_attr::unstructured
                  || domain->isWrittenCompressed(this->filename)
                  || isWrittenCompressedDomain(domId))
                continue;
            
              // unstructured grid seems not be taken into account why ?

              string lonName,latName ;

              if (domain->lon_name.isEmpty())
              { 
                if (domain->type==CDomain::type_attr::curvilinear) lonName = "nav_lon";
                else lonName = "lon";
              }
              else lonName = domain->lon_name;

              if (domain->lat_name.isEmpty())
              {
                if (domain->type==CDomain::type_attr::curvilinear) latName = "nav_lat";
                else latName = "lat";
              }
              else latName = domain->lat_name;
             
              StdString appendDomId  = singleDomain ? "" : "_" + domId;

              varId = domId + "_points";
              compress = latName + appendDomId + " " + lonName + appendDomId;
     
              shared_ptr<CLocalView> workflowView = domain->getLocalView(CElementView::WORKFLOW) ;
              workflowView->getGlobalIndexView(indexes) ;
              nbIndexes = workflowView->getSize() ;
              isDistributed = domain->isDistributed();
              if (isDistributed)
              {
                MPI_Exscan(&nbIndexes, &offset, 1, MPI_SIZE_T, MPI_SUM, comm_file) ;
                if (commRank==0) offset=0 ;
                MPI_Allreduce(&nbIndexes,&totalNbIndexes,1 , MPI_SIZE_T, MPI_SUM, comm_file) ;
              }
              else
              {
                offset=0 ;
                totalNbIndexes = nbIndexes ;
              }

              firstGlobalIndex = domain->ibeginValue_ + domain->jbeginValue_ * domain->ni_glo;

              domain->addRelFileCompressed(this->filename);
              setWrittenCompressedDomain(domId);
              ++idxDomain;
            }
            else if (1 == axisDomainOrder(i))
            {
              CAxis* axis = CAxis::get(axisList[idxAxis]);
              StdString axisId = axis->getAxisOutputName();

              if (!axis->isCompressible()
                  || axis->isWrittenCompressed(this->filename)
                  || isWrittenCompressedAxis(axisId))
                continue;

              varId = axisId + "_points";
              compress = axisId;

              shared_ptr<CLocalView> workflowView = axis->getLocalView(CElementView::WORKFLOW) ;
              workflowView->getGlobalIndexView(indexes) ;
              nbIndexes = workflowView->getSize() ;
              isDistributed = axis->isDistributed();
              if (isDistributed)
              {
                MPI_Exscan(&nbIndexes, &offset, 1, MPI_SIZE_T, MPI_SUM, comm_file) ;
                if (commRank==0) offset=0 ;
                MPI_Allreduce(&nbIndexes,&totalNbIndexes,1 , MPI_SIZE_T, MPI_SUM, comm_file) ;
              }
              else
              {
                offset=0 ;
                totalNbIndexes = nbIndexes ;
              }
              firstGlobalIndex = axis->begin;
              
              axis->addRelFileCompressed(this->filename);
              setWrittenCompressedAxis(axisId);
              ++idxAxis;
            }
            else
            {
              //for scalar 
            }

            if (!varId.empty())
            {
              SuperClassWriter::addDimension(varId, (SuperClass::type == MULTI_FILE) ? nbIndexes : totalNbIndexes);

              dims.clear();
              dims.push_back(varId);
              SuperClassWriter::addVariable(varId, NC_UINT64, dims);

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
        SuperClassWriter::addDimension(getTimeCounterName());
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

        std::vector<StdString> dims, coodinates;
        CGrid* grid = field->getGrid();
        if (!grid->doGridHaveDataToWrite())
          if (SuperClass::type==MULTI_FILE) return ;

        CArray<int,1> axisDomainOrder = grid->axis_domain_order;
        int numElement = axisDomainOrder.numElements(), idxDomain = 0, idxAxis = 0, idxScalar = 0;
        std::vector<StdString> domainList = grid->getDomainList();
        std::vector<StdString> axisList   = grid->getAxisList();
        std::vector<StdString> scalarList = grid->getScalarList();        

        StdString timeid  = getTimeCounterName();
        StdString dimXid,dimYid;
        std::deque<StdString> dimIdList, dimCoordList;
        bool hasArea = false;
        StdString cellMeasures = "area:";
        bool compressedOutput = !field->indexed_output.isEmpty() && field->indexed_output;

        for (int i = 0; i < numElement; ++i)
        {
          if (2 == axisDomainOrder(i))
          {
            CDomain* domain = CDomain::get(domainList[idxDomain]);
            StdString domId = domain->getDomainOutputName();
            StdString appendDomId  = singleDomain ? "" : "_" + domId ;
            StdString lonName,latName ;
            StdString dimIname,dimJname ;

            if (domain->lon_name.isEmpty())
            { 
              if (domain->type==CDomain::type_attr::curvilinear) lonName = "nav_lon";
              else lonName = "lon";
            }
            else lonName = domain->lon_name;

            if (domain->lat_name.isEmpty())
            {
              if (domain->type==CDomain::type_attr::curvilinear) latName = "nav_lat";
              else latName = "lat";
            }
            else latName = domain->lat_name;

            if (domain->dim_i_name.isEmpty())
            {
              if (domain->type==CDomain::type_attr::curvilinear) dimIname = "x";
              else if (domain->type==CDomain::type_attr::unstructured) dimIname = "cell";
              else dimIname = lonName;
            }
            else dimIname = domain->dim_i_name;

            if (domain->dim_j_name.isEmpty())
            {
              if (domain->type==CDomain::type_attr::curvilinear) dimJname = "y";
              else dimJname = latName;
            }
            else dimJname = domain->dim_j_name;
        
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
                  dimXid=dimIname+appendDomId;
                  dimYid=dimJname+appendDomId;
                  dimIdList.push_back(dimXid);
                  dimIdList.push_back(dimYid);
                }
                dimCoordList.push_back(lonName+appendDomId);
                dimCoordList.push_back(latName+appendDomId);
              break ;
              case CDomain::type_attr::rectilinear:
                if (!compressedOutput || !domain->isCompressible())
                {
                  dimXid     = dimIname+appendDomId;
                  dimYid     = dimJname+appendDomId;
                  dimIdList.push_back(dimXid);
                  dimIdList.push_back(dimYid);
                }
                if (lonName != dimIname)  dimCoordList.push_back(lonName+appendDomId);
                if (latName != dimJname)  dimCoordList.push_back(latName+appendDomId);

              break ;
              case CDomain::type_attr::unstructured:
              {
                if (SuperClassWriter::useCFConvention)
                {
                  dimXid     = dimIname + appendDomId;
                  dimIdList.push_back(dimXid);
                  dimCoordList.push_back(lonName+appendDomId);
                  dimCoordList.push_back(latName+appendDomId);
                }
                else
                {
                  StdString domainName = domain->name;
                  if (domain->nvertex == 1)
                  {
                    dimXid     = "n" + domainName + "_node";
                    dimIdList.push_back(dimXid);
                    dimCoordList.push_back(StdString(domainName + "_node_x"));
                    dimCoordList.push_back(StdString(domainName + "_node_y"));
                  }
                  else if (domain->nvertex == 2)
                  {
                    dimXid     = "n" + domainName + "_edge";
                    dimIdList.push_back(dimXid);
                    dimCoordList.push_back(StdString(domainName + "_edge_x"));
                    dimCoordList.push_back(StdString(domainName + "_edge_y"));
                  }
                  else
                  {
                    dimXid     = "n" + domainName + "_face";
                    dimIdList.push_back(dimXid);
                    dimCoordList.push_back(StdString(domainName + "_face_x"));
                    dimCoordList.push_back(StdString(domainName + "_face_y"));
                  }
                }  // ugrid convention
              }  // case unstructured domain
            }

            if (domain->hasArea)
            {
              hasArea = true;
              cellMeasures += " area" + appendDomId;
            }
            ++idxDomain;
          }
          else if (1 == axisDomainOrder(i))
          {
            CAxis* axis = CAxis::get(axisList[idxAxis]);
            StdString axisId = axis->getAxisOutputName();
            StdString axisDim;

            if (axis->dim_name.isEmpty()) axisDim = axisId;
            else axisDim=axis->dim_name.getValue();

            if (compressedOutput && axis->isCompressible())
            {
              dimIdList.push_back(axisDim + "_points");
              field->setUseCompressedOutput();
            }
            else
              dimIdList.push_back(axisDim);

            if (axisDim != axisId) dimCoordList.push_back(axisId);
            ++idxAxis;
          }
          else
          {
            CScalar* scalar = CScalar::get(scalarList[idxScalar]);
            StdString scalarId = scalar->getScalarOutputName();
            if (!scalar->value.isEmpty() || !scalar->label.isEmpty())
              dimCoordList.push_back(scalarId);
            ++idxScalar;
          }
        }

        StdString fieldid = field->getFieldOutputName();

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
          if (field->hasTimeInstant && hasTimeInstant) coodinates.push_back(string("time_instant"));
          else if (field->hasTimeCentered && hasTimeCentered)  coodinates.push_back(string("time_centered"));
          dims.push_back(timeid);
        }

        while (!dimIdList.empty())
        {
          dims.push_back(dimIdList.back());
          dimIdList.pop_back();
        }

        while (!dimCoordList.empty())
        {
          coodinates.push_back(dimCoordList.back());
          dimCoordList.pop_back();
        }

        try
        {
           SuperClassWriter::addVariable(fieldid, type, dims, false);
           SuperClassWriter::addChunk(field, type, dims);

           if (!field->standard_name.isEmpty())
              SuperClassWriter::addAttribute
                 ("standard_name",  field->standard_name.getValue(), &fieldid);

           if (!field->long_name.isEmpty())
              SuperClassWriter::addAttribute
                 ("long_name", field->long_name.getValue(), &fieldid);

           if (!field->unit.isEmpty())
              SuperClassWriter::addAttribute
                 ("units", field->unit.getValue(), &fieldid);

           // Ugrid field attributes "mesh" and "location"
           if (!SuperClassWriter::useCFConvention)
           {
            if (!domainList.empty())
            {
              CDomain* domain = CDomain::get(domainList[0]); // Suppose that we have only domain
              StdString mesh = domain->name;
              SuperClassWriter::addAttribute("mesh", mesh, &fieldid);
              StdString location;
              if (domain->nvertex == 1)
                location = "node";
              else if (domain->nvertex == 2)
                location = "edge";
              else if (domain->nvertex > 2)
                location = "face";
              SuperClassWriter::addAttribute("location", location, &fieldid);
            }

           }

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


           bool alreadyAddCellMethod = false;
           StdString cellMethodsPrefix(""), cellMethodsSuffix("");
           if (!field->cell_methods.isEmpty())
           {
              StdString cellMethodString = field->cell_methods;
              if (field->cell_methods_mode.isEmpty() ||
                 (CField::cell_methods_mode_attr::overwrite == field->cell_methods_mode))
              {
                SuperClassWriter::addAttribute("cell_methods", cellMethodString, &fieldid);
                alreadyAddCellMethod = true;
              }
              else
              {
                switch (field->cell_methods_mode)
                {
                  case (CField::cell_methods_mode_attr::prefix):
                    cellMethodsPrefix = cellMethodString;
                    cellMethodsPrefix += " ";
                    break;
                  case (CField::cell_methods_mode_attr::suffix):
                    cellMethodsSuffix = " ";
                    cellMethodsSuffix += cellMethodString;
                    break;
                  case (CField::cell_methods_mode_attr::none):
                    break;
                  default:
                    break;
                }
              }
           }


           if (wtime)
           {
              CDuration freqOp = field->freq_op.getValue();
              freqOp.solveTimeStep(*context->calendar);
              StdString freqOpStr = freqOp.toStringUDUnits();
              SuperClassWriter::addAttribute("interval_operation", freqOpStr, &fieldid);

              CDuration freqOut = field->getRelFile()->output_freq.getValue();
              freqOut.solveTimeStep(*context->calendar);
              SuperClassWriter::addAttribute("interval_write", freqOut.toStringUDUnits(), &fieldid);

              StdString cellMethods(cellMethodsPrefix + "time: ");
              if (field->operation.getValue() == "instant") cellMethods += "point";
              else if (field->operation.getValue() == "average") cellMethods += "mean";
              else if (field->operation.getValue() == "accumulate") cellMethods += "sum";
              else cellMethods += field->operation;
              if (freqOp.resolve(*context->calendar) != freqOut.resolve(*context->calendar))
                cellMethods += " (interval: " + freqOpStr + ")";
              cellMethods += cellMethodsSuffix;
              if (!alreadyAddCellMethod)
                SuperClassWriter::addAttribute("cell_methods", cellMethods, &fieldid);
           }

           if (hasArea)
             SuperClassWriter::addAttribute("cell_measures", cellMeasures, &fieldid);

           if (!field->default_value.isEmpty())
           {
             double default_value = field->default_value.getValue();
             if (type == NC_DOUBLE)
             {
               SuperClassWriter::setDefaultValue(fieldid, &default_value);
             }
             else if (type == NC_SHORT)
             {
               short sdefault_value = (short)default_value;
               SuperClassWriter::setDefaultValue(fieldid, &sdefault_value);
             }
             else
             {
               float fdefault_value = (float)default_value;
               SuperClassWriter::setDefaultValue(fieldid, &fdefault_value);
             }
           }
           else
              SuperClassWriter::setDefaultValue(fieldid, (double*)NULL);

            if (field->compression_level.isEmpty())
              field->compression_level = field->getRelFile()->compression_level.isEmpty() ? 0 : field->getRelFile()->compression_level;
            if ( (field->compression_level.getValue()>0)&&(field->compression_type.isEmpty()) )
              field->compression_type.setValue( "gzip" );
            if (field->compression_type.isEmpty())
              field->compression_type = "None";
            if (field->compression_params.isEmpty())
              field->compression_params.resize( 0 );
            SuperClassWriter::setCompressionLevel(fieldid, field->compression_type, field->compression_level, field->compression_params);

           {  // Ecriture des coordonnes

              StdString coordstr;
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

           vector<CVariable*> listVars = field->getAllVariables() ;
           for (vector<CVariable*>::iterator it = listVars.begin() ;it != listVars.end(); it++) writeAttribute_(*it, fieldid) ;

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
      } // writeField_()

      //--------------------------------------------------------------

      void CNc4DataOutput::writeFile_ (CFile* file)
      {
         StdString filename = file->getFileOutputName();
         StdString description = (!file->description.isEmpty())
                               ? file->description.getValue()
                               : StdString("Created by xios");

         singleDomain = (file->nbDomains == 1);

         StdString conv_str ;
         if (file->convention_str.isEmpty())
         {
            if (SuperClassWriter::useCFConvention) conv_str="CF-1.6" ;
            else conv_str="UGRID" ;
         }
         else conv_str=file->convention_str ;
           
         try
         {
           if (!appendMode) this->writeFileAttributes(filename, description,
                                                      conv_str,
                                                      StdString("An IPSL model"),
                                                      this->getTimeStamp());

           if (!appendMode)
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
        StdString name = var->getVariableOutputName();

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
        StdString name = var->getVariableOutputName();

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
         StdString formatStr;
         if (file->time_stamp_format.isEmpty()) formatStr="%Y-%b-%d %H:%M:%S %Z" ;
         else formatStr=file->time_stamp_format;

//         time ( &rawtime );
//         timeinfo = localtime ( &rawtime );
         time ( &rawtime );
         timeinfo = gmtime ( &rawtime );
         strftime (buffer, buffer_size, formatStr.c_str(), timeinfo);

         return (StdString(buffer));
      }

      //---------------------------------------------------------------

      int CNc4DataOutput::writeFieldData_ (CField*  field, const CArray<double,1>& data, const CDate& lastWrite,
                                           const CDate& currentWrite, int nstep)
      {
        CContext* context = CContext::getCurrent();
        CGrid* grid = field->getGrid();
       
        if (nstep<1) 
        {
          return nstep;
        }
        
        if (!grid->doGridHaveDataToWrite())
          if (SuperClass::type == MULTI_FILE || !isCollective)
          {
            return nstep;
          }

        StdString fieldid = field->getFieldOutputName();

        StdOStringStream oss;
        string timeAxisId;
        if (field->hasTimeInstant) timeAxisId = "time_instant";
        else if (field->hasTimeCentered) timeAxisId = "time_centered";

        StdString timeBoundId = getTimeCounterName() + "_bounds";

        StdString timeAxisBoundId;
        if (field->hasTimeInstant) timeAxisBoundId = "time_instant_bounds";
        else if (field->hasTimeCentered) timeAxisBoundId = "time_centered_bounds";

        if (!field->wasWritten())
        {
          if (appendMode && field->getRelFile()->record_offset.isEmpty() && 
              field->getOperationTimeType() != func::CFunctor::once)
          {
            double factorUnit;
            if (!field->getRelFile()->time_units.isEmpty() && field->getRelFile()->time_units==CFile::time_units_attr::days)
            factorUnit=context->getCalendar()->getDayLengthInSeconds() ;
            else factorUnit=1 ;
            nstep = getRecordFromTime(currentWrite,factorUnit) + 1;
          }

          field->setWritten();
        }


        CArray<double,1> time_data(1);
        CArray<double,1> time_data_bound(2);
        CArray<double,1> time_counter(1);
        CArray<double,1> time_counter_bound(2);

        bool wtime = (field->getOperationTimeType() != func::CFunctor::once);
        bool wtimeCounter =false ;
        bool wtimeData =false ;
        

        if (wtime)
        {
          
          if (field->hasTimeInstant)
          {
            time_data(0) = time_data_bound(1) = (Time) currentWrite;
            time_data_bound(0) = time_data_bound(1) = (Time) currentWrite;
            if (timeCounterType==instant)
            {
              time_counter(0) = time_data(0);
              time_counter_bound(0) = time_data_bound(0);
              time_counter_bound(1) = time_data_bound(1);
              wtimeCounter=true ;
            }
            if (hasTimeInstant) wtimeData=true ;
          }
          else if (field->hasTimeCentered)
          {
            time_data(0) = ((Time)currentWrite + (Time)lastWrite) / 2;
            time_data_bound(0) = (Time)lastWrite;
            time_data_bound(1) = (Time)currentWrite;
            if (timeCounterType==centered)
            {
              time_counter(0) = time_data(0) ;
              time_counter_bound(0) = time_data_bound(0) ;
              time_counter_bound(1) = time_data_bound(1) ;
              wtimeCounter=true ;
            }
            if (hasTimeCentered) wtimeData=true ;
          }

          if (timeCounterType==record)
          {
            time_counter(0) = nstep - 1;
            time_counter_bound(0) = time_counter_bound(1) = nstep - 1;
            wtimeCounter=true ;
          }

          if (!field->getRelFile()->time_units.isEmpty() && field->getRelFile()->time_units==CFile::time_units_attr::days)
          {
            double secByDay=context->getCalendar()->getDayLengthInSeconds() ;
            time_data/=secByDay;
            time_data_bound/=secByDay;
            time_counter/=secByDay;
            time_counter_bound/=secByDay;
          }
        }

         bool isRoot = (context->intraCommRank_ == 0);

         try
         {
           switch (SuperClass::type)
           {
              case (MULTI_FILE) :
              {
                 if (info.isActive(logProfile)) CTimer::get("Files : writing data").resume();
                 writeAndConvertData(field, data, nstep - 1);
                 if (info.isActive(logProfile)) CTimer::get("Files : writing data").suspend();
                 if (wtime)
                 {
                   if (info.isActive(logProfile)) CTimer::get("Files : writing time axis").resume();
                   if ( wtimeData)
                   {
                       SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, nstep - 1, isRoot);
                       SuperClassWriter::writeTimeAxisDataBounds(time_data_bound, timeAxisBoundId, isCollective, nstep - 1, isRoot);
                  }
                   if (wtimeCounter)
                   {
                     SuperClassWriter::writeTimeAxisData(time_counter, getTimeCounterName(), isCollective, nstep - 1,isRoot);
                     if (timeCounterType!=record) SuperClassWriter::writeTimeAxisDataBounds(time_counter_bound, timeBoundId, isCollective, nstep - 1, isRoot);
                   }
                   if (info.isActive(logProfile)) CTimer::get("Files : writing time axis").suspend();
                 }
                 break;
              }
              case (ONE_FILE) :
              {

                std::vector<StdSize> start, count;

                if (field->getUseCompressedOutput())
                {
                  CArray<int,1> axisDomainOrder = grid->axis_domain_order;
                  std::vector<StdString> domainList = grid->getDomainList();
                  std::vector<StdString> axisList   = grid->getAxisList();
                  int numElement = axisDomainOrder.numElements();
                  int idxDomain = domainList.size() - 1, idxAxis = axisList.size() - 1;
                  int idx = domainList.size() * 2 + axisList.size() - 1;
                  int commRank ;

                  MPI_Comm_rank(comm_file,&commRank) ;

                  start.reserve(idx+1);
                  count.reserve(idx+1);

                  for (int i = numElement - 1; i >= 0; --i)
                  {
                    if (2 == axisDomainOrder(i))
                    {
                      CDomain* domain = CDomain::get(domainList[idxDomain]);

                      if (domain->isCompressible())
                      {
                        size_t offset ;
                        size_t nbIndexes = domain->getLocalView(CElementView::WORKFLOW)->getSize() ;
                        if (domain->isDistributed())
                        {
                          MPI_Exscan(&nbIndexes, &offset, 1, MPI_SIZE_T, MPI_SUM, comm_file) ;
                          if (commRank==0) offset=0 ;
                        }
                        else offset=0 ;

                        start.push_back(offset);
                        count.push_back(nbIndexes);
                        idx -= 2;
                      }
                      else
                      {
                        if ((domain->type) != CDomain::type_attr::unstructured)
                        {
                          start.push_back(domain->jbeginValue_);
                          count.push_back(domain->njValue_);
                        }
                        --idx;
                        start.push_back(domain->ibeginValue_);
                        count.push_back(domain->niValue_);
                        --idx;
                      }
                      --idxDomain;
                    }
                    else if (1 == axisDomainOrder(i))
                    {
                      CAxis* axis = CAxis::get(axisList[idxAxis]);

                      if (axis->isCompressible())
                      {
                        size_t offset ;
                        size_t nbIndexes = axis->getLocalView(CElementView::WORKFLOW)->getSize() ;
                        if (axis->isDistributed())
                        {
                          MPI_Exscan(&nbIndexes, &offset, 1, MPI_SIZE_T, MPI_SUM, comm_file) ;
                          if (commRank==0) offset=0 ;
                        }
                        else offset=0 ;

                        start.push_back(offset);
                        count.push_back(nbIndexes);
                      }
                      else
                      {
                        start.push_back(axis->begin);
                        count.push_back(axis->n);
                      }
                      --idxAxis;
                      --idx;
                    }
                  }
                }
                else
                {
                  CArray<int,1> axisDomainOrder = grid->axis_domain_order;
                  std::vector<StdString> domainList = grid->getDomainList();
                  std::vector<StdString> axisList   = grid->getAxisList();
                  std::vector<StdString> scalarList  = grid->getScalarList() ;
                  int numElement = axisDomainOrder.numElements();
                  int idxDomain = domainList.size() - 1 ;
                  int idxAxis = axisList.size() - 1 ;
                  int idxScalar = scalarList.size() - 1 ;
                  int idx = domainList.size() * 2 + axisList.size() - 1;

                  start.reserve(idx+1);
                  count.reserve(idx+1);

                  for (int i = numElement - 1; i >= 0; --i)
                  {
                    if (2 == axisDomainOrder(i))
                    {
                      CDomain* domain = CDomain::get(domainList[idxDomain]);
                      if ((domain->type) != CDomain::type_attr::unstructured)
                      {
                        start.push_back(domain->jbeginValue_);
                        count.push_back(domain->njValue_);
                      }
                      --idx ;

                        start.push_back(domain->ibeginValue_);
                        count.push_back(domain->niValue_);
                      --idx ;
                      --idxDomain;
                    }
                    else if (1 == axisDomainOrder(i))
                    {
                        CAxis* axis = CAxis::get(axisList[idxAxis]);
                        start.push_back(axis->begin);
                        count.push_back(axis->n);
                      --idx;
                      --idxAxis;
                    }
                    else
                    {
                      if (idx ==-1) 
                      {
                        CScalar* scalar = CScalar::get(scalarList[idxScalar]);
                        start.push_back(0);
                        count.push_back(scalar->n);
                        idx-- ;
                      }
                      else if (idx<-1)
                      {
                        CScalar* scalar = CScalar::get(scalarList[idxScalar]);
                        count.back()*=scalar->n;
                        idx-- ;
                      }
                      --idxScalar;
                    }
                  }
                }


                if (info.isActive(logProfile)) CTimer::get("Files : writing data").resume();
                writeAndConvertData(field, data, nstep - 1, &start, &count);
                if (info.isActive(logProfile)) CTimer::get("Files : writing data").suspend();

                 if (wtime)
                 {
                   if (info.isActive(logProfile)) CTimer::get("Files : writing time axis").resume();
                   if ( wtimeData)
                   {
                     SuperClassWriter::writeTimeAxisData(time_data, timeAxisId, isCollective, nstep - 1, isRoot);
                     SuperClassWriter::writeTimeAxisDataBounds(time_data_bound, timeAxisBoundId, isCollective, nstep - 1, isRoot);
                   }
                   if (wtimeCounter)
                   {
                     SuperClassWriter::writeTimeAxisData(time_counter, getTimeCounterName(), isCollective, nstep - 1,isRoot);
                     if (timeCounterType!=record) SuperClassWriter::writeTimeAxisDataBounds(time_counter_bound, timeBoundId, isCollective, nstep - 1, isRoot);

                   }
                   if (info.isActive(logProfile)) CTimer::get("Files : writing time axis").suspend();  
                 }

                break;
              }
            }
            return nstep ;
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

      void CNc4DataOutput::writeAndConvertData(CField* field, const CArray<double,1>& data, StdSize record, const std::vector<StdSize> *start, const std::vector<StdSize> *count)
      {
        StdString fieldid = field->getFieldOutputName();
        nc_type type ;
        if (field->prec.isEmpty()) type =  NC_FLOAT ;
        else
        {
          if (field->prec==2) type = NC_SHORT ;
          else if (field->prec==4)  type =  NC_FLOAT ;
          else if (field->prec==8)   type =  NC_DOUBLE ;
        }

        bool conversionByNetCDF = true; // default : conversion operated by NetCDF for now (legacy behaviour)
        if (!field->conversion_by_netcdf.isEmpty())
        {
          // use conversion_by_netcdf = ".false." to  bypass NetCDF conversion
          //   poor performances from NC_DOUBLE to NC_FLOAT with isgreater/isless in recent NetCDF available at TGCC
          conversionByNetCDF = field->conversion_by_netcdf;
        }
        if ( ( type == NC_DOUBLE ) || ( conversionByNetCDF ) )
        {
          SuperClassWriter::writeData(data, fieldid, isCollective, record, start, count);
        }
        else if ( type == NC_FLOAT )
        {
          CArray<float,1> f_data;
          f_data.resize( data.numElements() );
          for (int i = 0; i < data.numElements(); i++) 
          {
            if (data(i) <= X_FLOAT_MAX || data(i) >= X_FLOAT_MIN)
              f_data(i) = data(i);
            else if (data(i) > X_FLOAT_MAX)
              f_data(i) = X_FLOAT_MAX;
            else if (data(i) < X_FLOAT_MIN)
              f_data(i) = X_FLOAT_MIN;
          }
          SuperClassWriter::writeData(f_data, fieldid, isCollective, record, start, count);
        }
        else if ( type == NC_SHORT )
        {
          CArray<short,1> s_data;
          s_data.resize( data.numElements() );
          for (int i = 0; i < data.numElements(); i++) 
          {
            if (data(i) <= X_SHORT_MAX || data(i) >= X_SHORT_MIN)
              s_data(i) = data(i);
            else if (data(i) > X_SHORT_MAX)
              s_data(i) = X_SHORT_MAX;
            else if (data(i) < X_SHORT_MIN)
              s_data(i) = X_SHORT_MIN;
          }
          SuperClassWriter::writeData(s_data, fieldid, isCollective, record, start, count);
        }
        else
        {
            ERROR("CNc4DataOutput::writeAndConvertData(...)", << "Type conversion not managed for " << fieldid ); 
        }
        
      }

      //---------------------------------------------------------------

      void CNc4DataOutput::writeTimeAxis_
                  (CField*    field,
                   const std::shared_ptr<CCalendar> cal)
      {
         StdOStringStream oss;
         bool createInstantAxis=false ;
         bool createCenteredAxis=false ;
         bool createTimeCounterAxis=false ;
         
         if (field->getOperationTimeType() == func::CFunctor::once) return ;


         StdString axisId ;
         StdString axisBoundId;
         StdString timeid(getTimeCounterName());
         StdString timeBoundId("axis_nbounds");

         StdString strTimeUnits ;
         if (!field->getRelFile()->time_units.isEmpty() && field->getRelFile()->time_units==CFile::time_units_attr::days) strTimeUnits="days since " ;
         else  strTimeUnits="seconds since " ;
 
         if (field->getOperationTimeType() == func::CFunctor::instant) field->hasTimeInstant = true;
         if (field->getOperationTimeType() == func::CFunctor::centered) field->hasTimeCentered = true;


         if (field->getRelFile()->time_counter.isEmpty())
         {
           if (timeCounterType==none) createTimeCounterAxis=true ;
           if (field->hasTimeCentered)
           {
             timeCounterType=centered ;
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none) timeCounterType=instant ;
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::instant)
         {
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=instant ;
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::centered)
         {
           if (field->hasTimeCentered)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=centered ;
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::instant_exclusive)
         {
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=instant ;
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::centered_exclusive)
         {
           if (field->hasTimeCentered)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             timeCounterType=centered ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::exclusive)
         {
           if (field->hasTimeCentered)
           {
             if (timeCounterType==none) createTimeCounterAxis=true ;
             if (timeCounterType==instant) createInstantAxis=true ;
             timeCounterType=centered ;
           }
           if (field->hasTimeInstant)
           {
             if (timeCounterType==none)
             {
               createTimeCounterAxis=true ;
               timeCounterType=instant ;
             }
             if (timeCounterType==centered)
             {
               if (!hasTimeInstant) createInstantAxis=true ;
             }
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::none)
         {
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         else if (field->getRelFile()->time_counter==CFile::time_counter_attr::record)
         {
           if (timeCounterType==none) createTimeCounterAxis=true ;
           timeCounterType=record ;
           if (field->hasTimeCentered)
           {
             if (!hasTimeCentered) createCenteredAxis=true ;
           }
           if (field->hasTimeInstant)
           {
             if (!hasTimeInstant) createInstantAxis=true ;
           }
         }
         
         if (createInstantAxis)
         {
           axisId="time_instant" ;
           axisBoundId="time_instant_bounds";
           hasTimeInstant=true ;
         }

         if (createCenteredAxis)
         {
           axisId="time_centered" ;
           axisBoundId="time_centered_bounds";
           hasTimeCentered=true ;
         }

         
         try
         {
            std::vector<StdString> dims;
            
            if (createInstantAxis || createCenteredAxis)
            {
              // Adding time_instant or time_centered
              dims.push_back(timeid);
              if (!SuperClassWriter::varExist(axisId))
              {
                SuperClassWriter::addVariable(axisId, NC_DOUBLE, dims);

                CDate timeOrigin=cal->getTimeOrigin() ;
                StdOStringStream oss2;
                StdString strInitdate=oss2.str() ;
                StdString strTimeOrigin=timeOrigin.toString() ;
                this->writeTimeAxisAttributes(axisId, cal->getType(),strTimeUnits+strTimeOrigin,
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
           }

           if (createTimeCounterAxis)
           {
             // Adding time_counter
             axisId = getTimeCounterName();
             axisBoundId = getTimeCounterName() + "_bounds";
             dims.clear();
             dims.push_back(timeid);
             if (!SuperClassWriter::varExist(axisId))
             {
                SuperClassWriter::addVariable(axisId, NC_DOUBLE, dims);
                SuperClassWriter::addAttribute("axis", string("T"), &axisId);

                if (field->getRelFile()->time_counter.isEmpty() || 
                   (field->getRelFile()->time_counter != CFile::time_counter_attr::record))
                {
                  CDate timeOrigin = cal->getTimeOrigin();
                  StdString strTimeOrigin = timeOrigin.toString();

                  this->writeTimeAxisAttributes(axisId, cal->getType(),
                                                strTimeUnits+strTimeOrigin,
                                                strTimeOrigin, axisBoundId);
                }
             }

             // Adding time_counter_bound dimension
             if (field->getRelFile()->time_counter.isEmpty() || (field->getRelFile()->time_counter != CFile::time_counter_attr::record))
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
                  const std::shared_ptr<CCalendar> cal)", << msg);
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
//          SuperClassWriter::addAttribute("nav_model"    , nav_model    , &axis_name);
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
           array=ibegin+1,jbegin+1 ;
           SuperClassWriter::addAttribute("DOMAIN_position_first", array) ;
           array=ibegin+ni-1+1,jbegin+nj-1+1 ;
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
           // SuperClassWriter::addAttribute("production" , production);

           StdString timeStampStr ;
           if (file->time_stamp_name.isEmpty()) timeStampStr="timeStamp" ;
           else timeStampStr=file->time_stamp_name ;
           SuperClassWriter::addAttribute(timeStampStr, timeStamp);

           StdString uuidName ;
           if (file->uuid_name.isEmpty()) uuidName="uuid" ;
           else uuidName=file->uuid_name ;

           if (file->uuid_format.isEmpty()) SuperClassWriter::addAttribute(uuidName, getUuidStr());
           else SuperClassWriter::addAttribute(uuidName, getUuidStr(file->uuid_format));
          
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

      StdSize CNc4DataOutput::getRecordFromTime(Time time, double factorUnit)
      {
        std::map<Time, StdSize>::const_iterator it = timeToRecordCache.find(time);
        if (it == timeToRecordCache.end())
        {
          StdString timeAxisBoundsId(getTimeCounterName() + "_bounds");
          if (!SuperClassWriter::varExist(timeAxisBoundsId)) timeAxisBoundsId = "time_centered_bounds";
          if (!SuperClassWriter::varExist(timeAxisBoundsId)) timeAxisBoundsId = "time_instant_bounds";

          CArray<double,2> timeAxisBounds;
          std::vector<StdSize> dimSize(SuperClassWriter::getDimensions(timeAxisBoundsId)) ;
          
          StdSize record = 0;
          double dtime(time);
          for (int n = dimSize[0] - 1; n >= 0; n--)
          {
            SuperClassWriter::getTimeAxisBounds(timeAxisBounds, timeAxisBoundsId, isCollective, n);
            timeAxisBounds*=factorUnit ;
            if (timeAxisBounds(1, 0) < dtime)
            {
              record = n + 1;
              break;
            }
          }
          it = timeToRecordCache.insert(std::make_pair(time, record)).first;
        }
        return it->second;
      }

      ///--------------------------------------------------------------

      bool CNc4DataOutput::isWrittenDomain(const std::string& domainName) const
      {
        return (this->writtenDomains.find(domainName) != this->writtenDomains.end());
      }

      bool CNc4DataOutput::isWrittenCompressedDomain(const std::string& domainName) const
      {
        return (this->writtenCompressedDomains.find(domainName) != this->writtenCompressedDomains.end());
      }

      bool CNc4DataOutput::isWrittenAxis(const std::string& axisName) const
      {
        return (this->writtenAxis.find(axisName) != this->writtenAxis.end());
      }

      bool CNc4DataOutput::isWrittenCompressedAxis(const std::string& axisName) const
      {
        return (this->writtenCompressedAxis.find(axisName) != this->writtenCompressedAxis.end());
      }

      bool CNc4DataOutput::isWrittenScalar(const std::string& scalarName) const
      {
        return (this->writtenScalar.find(scalarName) != this->writtenScalar.end());
      }

      void CNc4DataOutput::setWrittenDomain(const std::string& domainName)
      {
        this->writtenDomains.insert(domainName);
      }

      void CNc4DataOutput::setWrittenCompressedDomain(const std::string& domainName)
      {
        this->writtenCompressedDomains.insert(domainName);
      }

      void CNc4DataOutput::setWrittenAxis(const std::string& axisName)
      {
        this->writtenAxis.insert(axisName);
      }

      void CNc4DataOutput::setWrittenCompressedAxis(const std::string& axisName)
      {
        this->writtenCompressedAxis.insert(axisName);
      }

      void CNc4DataOutput::setWrittenScalar(const std::string& scalarName)
      {
        this->writtenScalar.insert(scalarName);
      }
} // namespace xios
