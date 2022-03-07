#include "domain.hpp"
#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"

#include "xios_spl.hpp"
#include "event_client.hpp"
#include "event_server.hpp"
#include "buffer_in.hpp"
#include "message.hpp"
#include "type.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "context_server.hpp"
#include "array_new.hpp"
#include "distribution_client.hpp"
#include "server_distribution_description.hpp"
#include "client_server_mapping_distributed.hpp"
#include "local_connector.hpp"
#include "grid_local_connector.hpp"
#include "remote_connector.hpp"
#include "gatherer_connector.hpp"
#include "scatterer_connector.hpp"
#include "grid_scatterer_connector.hpp"
#include "grid_gatherer_connector.hpp"
#include "transformation_path.hpp"
#include "grid_transformation_factory_impl.hpp"

#include <algorithm>
#include <regex>


namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CDomain::CDomain(void)
      : CObjectTemplate<CDomain>(), CDomainAttributes()
      , isChecked(false), relFiles(),  indSrv_(), connectedServerRank_()
      , hasBounds(false), hasArea(false), isCompressible_(false), isUnstructed_(false)
      , hasLonLat(false)
      , isRedistributed_(false), hasPole(false)
      , lonvalue(), latvalue(), bounds_lonvalue(), bounds_latvalue()
      , clients(), hasLatInReadFile_(false), hasBoundsLatInReadFile_(false)
      , hasLonInReadFile_(false), hasBoundsLonInReadFile_(false)
   {
   }

   CDomain::CDomain(const StdString & id)
      : CObjectTemplate<CDomain>(id), CDomainAttributes()
      , isChecked(false), relFiles(), indSrv_(), connectedServerRank_() 
      , hasBounds(false), hasArea(false), isCompressible_(false), isUnstructed_(false)
      , hasLonLat(false)
      , isRedistributed_(false), hasPole(false)
      , lonvalue(), latvalue(), bounds_lonvalue(), bounds_latvalue()
      , clients(), hasLatInReadFile_(false), hasBoundsLatInReadFile_(false)
      , hasLonInReadFile_(false), hasBoundsLonInReadFile_(false)
   {
    }

   CDomain::~CDomain(void)
   {
   }

   void CDomain::releaseStaticAllocation(void)
   {
     transformationMapList_.clear() ;
     CTransformation<CDomain>::unregisterAllTransformations() ;
     CGridTransformationFactory<CDomain>::unregisterAllTransformations() ;
   }
   ///---------------------------------------------------------------

   void CDomain::assignMesh(const StdString meshName, const int nvertex)
   TRY
   {
     mesh = CMesh::getMesh(meshName, nvertex);
   }
   CATCH_DUMP_ATTR

   CDomain* CDomain::createDomain()
   TRY
   {
     CDomain* domain = CDomainGroup::get("domain_definition")->createChild();
     return domain;
   }
   CATCH

   CDomain* CDomain::get(const string& id, bool noError)
   {
     const regex r("::");
     smatch m;
     if (regex_search(id, m, r))
     {
        if (m.size()!=1) ERROR("CDomain* CDomain::get(string& id)", <<" id = "<<id<< "  -> bad format id, separator :: append more than one time");
        string fieldId=m.prefix() ;
        if (fieldId.empty()) ERROR("CDomain* CDomain::get(string& id)", <<" id = "<<id<< "  -> bad format id, field name is empty");
        string suffix=m.suffix() ;
        if (!CField::has(fieldId)) 
          if (noError)  return nullptr ;
          else ERROR("CDomain* CDomain::get(string& id)", <<" id = "<<id<< "  -> field Id : < "<<fieldId<<" > doesn't exist");
        CField* field=CField::get(fieldId) ;
        return field->getAssociatedDomain(suffix, noError) ;
     }
     else 
     {
       if (noError) if(!CObjectFactory::HasObject<CDomain>(id)) return nullptr ;
       return CObjectFactory::GetObject<CDomain>(id).get();
     }
   }

   bool CDomain::has(const string& id)
   {
     if (CDomain::get(id,true)==nullptr) return false ;
     else return true ;
   }
   
   CField* CDomain::getFieldFromId(const string& id)
   {
     const regex r("::");
     smatch m;
     if (regex_search(id, m, r))
     {
        if (m.size()!=1) ERROR("CField* CDomain::getFieldFromId(const string& id)", <<" id = "<<id<< "  -> bad format id, separator :: append more than one time");
        string fieldId=m.prefix() ;
        if (fieldId.empty()) ERROR("CField* CDomain::getFieldFromId(const string& id)", <<" id = "<<id<< "  -> bad format id, field name is empty");
        string suffix=m.suffix() ;
        CField* field=CField::get(fieldId) ;
        return field ;
     }
     else return nullptr;
   }

   const std::set<StdString> & CDomain::getRelFiles(void) const
   TRY
   {
      return (this->relFiles);
   }
   CATCH

     //----------------------------------------------------------------

   /*!
    * Compute the minimum buffer size required to send the attributes to the server(s).
    *
    * \return A map associating the server rank with its minimum buffer size.
    */
   std::map<int, StdSize> CDomain::getAttributesBufferSize(CContextClient* client, bool bufferForWriting /*= false*/)
   TRY
   {

     std::map<int, StdSize> attributesSizes = getMinimumBufferSizeForAttributes(client);

     if (client->isServerLeader())
     {
       // size estimation for sendDistributionAttribut
       size_t size = 11 * sizeof(size_t);

       const std::list<int>& ranks = client->getRanksServerLeader();
       for (std::list<int>::const_iterator itRank = ranks.begin(), itRankEnd = ranks.end(); itRank != itRankEnd; ++itRank)
       {
         if (size > attributesSizes[*itRank])
           attributesSizes[*itRank] = size;
       }
     }

     std::unordered_map<int, vector<size_t> >::const_iterator itIndexEnd = indSrv_[client->serverSize].end();
     // std::map<int, std::vector<int> >::const_iterator itWrittenIndexEnd = indWrittenSrv_.end();
     for (size_t k = 0; k < connectedServerRank_[client->serverSize].size(); ++k)
     {
       int rank = connectedServerRank_[client->serverSize][k];
       std::unordered_map<int, std::vector<size_t> >::const_iterator it = indSrv_[client->serverSize].find(rank);
       size_t idxCount = (it != itIndexEnd) ? it->second.size() : 0;

       // size estimation for sendIndex (and sendArea which is always smaller or equal)
       size_t sizeIndexEvent = 2 * sizeof(size_t) + 2 * CArray<int,1>::size(idxCount);

       // size estimation for sendLonLat
       size_t sizeLonLatEvent = CArray<double,1>::size(idxCount);
       if (hasBounds)
         sizeLonLatEvent += CArray<double,2>::size(nvertex * idxCount);

       size_t size = CEventClient::headerSize + getId().size() + sizeof(size_t) + std::max(sizeIndexEvent, sizeLonLatEvent);
       if (size > attributesSizes[rank])
         attributesSizes[rank] = size;
     }

     return attributesSizes;
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   bool CDomain::isEmpty(void) const
   TRY
   {
     return ((this->i_index.isEmpty()) || (0 == this->i_index.numElements()));
   }
   CATCH

   //----------------------------------------------------------------

   bool CDomain::IsWritten(const StdString & filename) const
   TRY
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }
   CATCH

   bool CDomain::isWrittenCompressed(const StdString& filename) const
   TRY
   {
      return (this->relFilesCompressed.find(filename) != this->relFilesCompressed.end());
   }
   CATCH

   //----------------------------------------------------------------

   bool CDomain::isDistributed(void) const
   TRY
   {
      bool distributed =  !((!ni.isEmpty() && (ni == ni_glo) && !nj.isEmpty() && (nj == nj_glo)) ||
              (!i_index.isEmpty() && i_index.numElements() == ni_glo*nj_glo));
      bool distributed_glo ;
      distributed |= (1 == CContext::getCurrent()->intraCommSize_);

      return distributed;
   }
   CATCH

   //----------------------------------------------------------------

   /*!
    * Compute if the domain can be ouput in a compressed way.
    * In this case the workflow view on server side must be the same
    * than the full view for all context rank. The result is stored on
    * internal isCompressible_ attribute.
    */
   void CDomain::computeIsCompressible(void)
   TRY
   {
     // mesh is compressible contains some masked or indexed value, ie if full view is different of workflow view.
     // But now assume that the size of the 2 view must be equal for everybody. True on server side
     int isSameView = getLocalView(CElementView::FULL)->getSize() ==  getLocalView(CElementView::WORKFLOW)->getSize();
     MPI_Allreduce(MPI_IN_PLACE, &isSameView, 1, MPI_INT, MPI_LAND, CContext::getCurrent()->getIntraComm()) ;
     if (isSameView) isCompressible_ = false ;
     else isCompressible_ = true ;
     isCompressibleComputed_=true ;
   }
   CATCH

   void CDomain::addRelFile(const StdString & filename)
   TRY
   {
      this->relFiles.insert(filename);
   }
   CATCH_DUMP_ATTR

   void CDomain::addRelFileCompressed(const StdString& filename)
   TRY
   {
      this->relFilesCompressed.insert(filename);
   }
   CATCH_DUMP_ATTR

   StdString CDomain::GetName(void)   { return (StdString("domain")); }
   StdString CDomain::GetDefName(void){ return (CDomain::GetName()); }
   ENodeType CDomain::GetType(void)   { return (eDomain); }

   //----------------------------------------------------------------

   /*!
      Verify if all distribution information of a domain are available
      This checking verifies the definition of distribution attributes (ni, nj, ibegin, jbegin)
   */
   bool CDomain::distributionAttributesHaveValue() const
   TRY
   {
      bool hasValues = true;

      if (ni.isEmpty() && ibegin.isEmpty() && i_index.isEmpty())
      {
        hasValues = false;
        return hasValues;
      }

      return hasValues;
   }
   CATCH

   /*!
     Redistribute RECTILINEAR or CURVILINEAR domain with a number of local domains.
   All attributes ni,nj,ibegin,jbegin (if defined) will be rewritten
   The optional attributes lonvalue, latvalue will be added. Because this function only serves (for now)
   for interpolation from unstructured domain to rectilinear one, range of latvalue is 0-360 and lonvalue is -90 - +90
    \param [in] nbLocalDomain number of local domain on the domain destination
   */
   void CDomain::redistribute(int nbLocalDomain)
   TRY
   {
     if (this->isRedistributed_) return;

     this->isRedistributed_ = true;
     CContext* context = CContext::getCurrent();
     // For now the assumption is that secondary server pools consist of the same number of procs.
     // CHANGE the line below if the assumption changes.

     int rankClient = context->intraCommRank_;
     int rankOnDomain = rankClient%nbLocalDomain;

     if (ni_glo.isEmpty() || ni_glo <= 0 )
     {
        ERROR("CDomain::redistribute(int nbLocalDomain)",
           << "[ Id = " << this->getId() << " ] "
           << "The global domain is badly defined,"
           << " check the \'ni_glo\'  value !")
     }

     if (nj_glo.isEmpty() || nj_glo <= 0 )
     {
        ERROR("CDomain::redistribute(int nbLocalDomain)",
           << "[ Id = " << this->getId() << " ] "
           << "The global domain is badly defined,"
           << " check the \'nj_glo\'  value !")
     }

     if ((type_attr::rectilinear == type)  || (type_attr::curvilinear == type))
     {
        int globalDomainSize = ni_glo * nj_glo;
        if (globalDomainSize <= nbLocalDomain)
        {
          for (int idx = 0; idx < nbLocalDomain; ++idx)
          {
            if (rankOnDomain < globalDomainSize)
            {
              int iIdx = rankOnDomain % ni_glo;
              int jIdx = rankOnDomain / ni_glo;
              ibegin.setValue(iIdx); jbegin.setValue(jIdx);
              ni.setValue(1); nj.setValue(1);
            }
            else
            {
              ibegin.setValue(0); jbegin.setValue(0);
              ni.setValue(0); nj.setValue(0);
            }
          }
        }
        else
        {
          float njGlo = nj_glo.getValue();
          float niGlo = ni_glo.getValue();
          int nbProcOnX, nbProcOnY, range;

          // Compute (approximately) number of segment on x and y axis
          float yOverXRatio = njGlo/niGlo;

          nbProcOnX = std::ceil(std::sqrt(nbLocalDomain/yOverXRatio));
          nbProcOnY = std::ceil(((float)nbLocalDomain)/nbProcOnX);

          // Simple distribution: Sweep from top to bottom, left to right
          // Calculate local begin on x
          std::vector<int> ibeginVec(nbProcOnX,0), jbeginVec(nbProcOnY,0);
          std::vector<int> niVec(nbProcOnX), njVec(nbProcOnY);
          for (int i = 1; i < nbProcOnX; ++i)
          {
            range = ni_glo / nbProcOnX;
            if (i < (ni_glo%nbProcOnX)) ++range;
            niVec[i-1] = range;
            ibeginVec[i] = ibeginVec[i-1] + niVec[i-1];
          }
          niVec[nbProcOnX-1] = ni_glo - ibeginVec[nbProcOnX-1];

          // Calculate local begin on y
          for (int j = 1; j < nbProcOnY; ++j)
          {
            range = nj_glo / nbProcOnY;
            if (j < (nj_glo%nbProcOnY)) ++range;
            njVec[j-1] = range;
            jbeginVec[j] = jbeginVec[j-1] + njVec[j-1];
          }
          njVec[nbProcOnY-1] = nj_glo - jbeginVec[nbProcOnY-1];

          // Now assign value to ni, ibegin, nj, jbegin
          int iIdx = rankOnDomain % nbProcOnX;
          int jIdx = rankOnDomain / nbProcOnX;

          if (rankOnDomain != (nbLocalDomain-1))
          {
            ibegin.setValue(ibeginVec[iIdx]);
            jbegin.setValue(jbeginVec[jIdx]);
            nj.setValue(njVec[jIdx]);
            ni.setValue(niVec[iIdx]);
          }
          else // just merge all the remaining rectangle into the last one
          {
            ibegin.setValue(ibeginVec[iIdx]);
            jbegin.setValue(jbeginVec[jIdx]);
            nj.setValue(njVec[jIdx]);
            ni.setValue(ni_glo - ibeginVec[iIdx]);
          }
        } 
     }
     else  // unstructured domain
     {
       if (this->i_index.isEmpty())
       {
          int globalDomainSize = ni_glo * nj_glo;
          if (globalDomainSize <= nbLocalDomain)
          {
            for (int idx = 0; idx < nbLocalDomain; ++idx)
            {
              if (rankOnDomain < globalDomainSize)
              {
                int iIdx = rankOnDomain % ni_glo;
                int jIdx = rankOnDomain / ni_glo;
                ibegin.setValue(iIdx); jbegin.setValue(jIdx);
                ni.setValue(1); nj.setValue(1);
              }
              else
              {
                ibegin.setValue(0); jbegin.setValue(0);
                ni.setValue(0); nj.setValue(0);
              }
            }
          }
          else
          {
            float njGlo = nj_glo.getValue();
            float niGlo = ni_glo.getValue();
            std::vector<int> ibeginVec(nbLocalDomain,0);
            std::vector<int> niVec(nbLocalDomain);
            for (int i = 1; i < nbLocalDomain; ++i)
            {
              int range = ni_glo / nbLocalDomain;
              if (i < (ni_glo%nbLocalDomain)) ++range;
              niVec[i-1] = range;
              ibeginVec[i] = ibeginVec[i-1] + niVec[i-1];
            }
            niVec[nbLocalDomain-1] = ni_glo - ibeginVec[nbLocalDomain-1];

            int iIdx = rankOnDomain % nbLocalDomain;
            ibegin.setValue(ibeginVec[iIdx]);
            jbegin.setValue(0);
            ni.setValue(niVec[iIdx]);
            nj.setValue(1);
          }

          i_index.resize(ni);          
          for(int idx = 0; idx < ni; ++idx) i_index(idx)=ibegin+idx;
        }
        else
        {
          ibegin.setValue(this->i_index(0));
          jbegin.setValue(0);
          ni.setValue(this->i_index.numElements());
          nj.setValue(1);
        }
     }

     checkDomain();
   }
   CATCH_DUMP_ATTR

   /*!
     Fill in longitude and latitude whose values are read from file
   */
   void CDomain::fillInLonLat()
   TRY
   {
     switch (type)
     {
      case type_attr::rectilinear:
        fillInRectilinearLonLat();
        break;
      case type_attr::curvilinear:
        fillInCurvilinearLonLat();
        break;
      case type_attr::unstructured:
        fillInUnstructuredLonLat();
        break;

      default:
      break;
     }
     completeLonLatClient() ;
   }
   CATCH_DUMP_ATTR

   /*!
     Fill in the values for lonvalue_1d and latvalue_1d of rectilinear domain
     Range of longitude value from 0 - 360
     Range of latitude value from -90 - +90
   */
   void CDomain::fillInRectilinearLonLat()
   TRY
   {
     if (!lonvalue_rectilinear_read_from_file.isEmpty() && lonvalue_2d.isEmpty() && lonvalue_1d.isEmpty())
     {
       lonvalue_1d.resize(ni);
       for (int idx = 0; idx < ni; ++idx)
         lonvalue_1d(idx) = lonvalue_rectilinear_read_from_file(idx+ibegin);
       lon_start.setValue(lonvalue_rectilinear_read_from_file(0));
       lon_end.setValue(lonvalue_rectilinear_read_from_file(ni_glo-1));
     }
     else if (!hasLonInReadFile_)
     {
       if (!lonvalue_2d.isEmpty()) lonvalue_2d.free();
       lonvalue_1d.resize(ni);
       double lonRange = lon_end - lon_start;
       double lonStep = (1 == ni_glo.getValue()) ? lonRange : lonRange/double(ni_glo.getValue()-1);

        // Assign lon value
       for (int i = 0; i < ni; ++i)
       {
         if (0 == (ibegin + i))
         {
           lonvalue_1d(i) = lon_start;
         }
         else if (ni_glo == (ibegin + i + 1))
         {
           lonvalue_1d(i) = lon_end;
         }
         else
         {
           lonvalue_1d(i) = (ibegin + i) * lonStep  + lon_start;
         }
       }
     }


     if (!latvalue_rectilinear_read_from_file.isEmpty() && latvalue_2d.isEmpty() && latvalue_1d.isEmpty())
     {
       latvalue_1d.resize(nj);
       for (int idx = 0; idx < nj; ++idx)
         latvalue_1d(idx) = latvalue_rectilinear_read_from_file(idx+jbegin);
       lat_start.setValue(latvalue_rectilinear_read_from_file(0));
       lat_end.setValue(latvalue_rectilinear_read_from_file(nj_glo-1));
     }
     else if (!hasLatInReadFile_)
     {
       if (!latvalue_2d.isEmpty()) latvalue_1d.free();
       latvalue_1d.resize(nj);

       double latRange = lat_end - lat_start;
       double latStep = (1 == nj_glo.getValue()) ? latRange : latRange/double(nj_glo.getValue()-1);

       for (int j = 0; j < nj; ++j)
       {
         if (0 == (jbegin + j))
         {
            latvalue_1d(j) = lat_start;
         }
         else if (nj_glo == (jbegin + j + 1))
         {
            latvalue_1d(j) = lat_end;
         }
         else
         {
           latvalue_1d(j) =  (jbegin + j) * latStep + lat_start;
         }
       }
     }
   }
   CATCH_DUMP_ATTR

    /*
      Fill in 2D longitude and latitude of curvilinear domain read from a file.
      If there are already longitude and latitude defined by model. We just ignore read value.
    */
   void CDomain::fillInCurvilinearLonLat()
   TRY
   {
     if (!lonvalue_curvilinear_read_from_file.isEmpty() && lonvalue_2d.isEmpty() && lonvalue_1d.isEmpty())
     {
       lonvalue_2d.resize(ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
         lonvalue_2d(idx,jdx) = lonvalue_curvilinear_read_from_file(idx, jdx);

       lonvalue_curvilinear_read_from_file.free();
     }

     if (!latvalue_curvilinear_read_from_file.isEmpty() && latvalue_2d.isEmpty() && latvalue_1d.isEmpty())
     {
       latvalue_2d.resize(ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
           latvalue_2d(idx,jdx) = latvalue_curvilinear_read_from_file(idx, jdx);

       latvalue_curvilinear_read_from_file.free();
     }

     if (!bounds_lonvalue_curvilinear_read_from_file.isEmpty() && bounds_lon_2d.isEmpty() && bounds_lon_1d.isEmpty())
     {
       bounds_lon_2d.resize(nvertex,ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
          for (int ndx = 0; ndx < nvertex; ++ndx)
            bounds_lon_2d(ndx,idx,jdx) = bounds_lonvalue_curvilinear_read_from_file(ndx,idx, jdx);

       bounds_lonvalue_curvilinear_read_from_file.free();
     }

     if (!bounds_latvalue_curvilinear_read_from_file.isEmpty() && bounds_lat_2d.isEmpty() && bounds_lat_1d.isEmpty())
     {
       bounds_lat_2d.resize(nvertex,ni,nj);
       for (int jdx = 0; jdx < nj; ++jdx)
        for (int idx = 0; idx < ni; ++idx)
          for (int ndx = 0; ndx < nvertex; ++ndx)
            bounds_lat_2d(ndx,idx,jdx) = bounds_latvalue_curvilinear_read_from_file(ndx,idx, jdx);

       bounds_latvalue_curvilinear_read_from_file.free();
     }
   }
   CATCH_DUMP_ATTR

    /*
      Fill in longitude and latitude of unstructured domain read from a file
      If there are already longitude and latitude defined by model. We just igonore reading value.
    */
   void CDomain::fillInUnstructuredLonLat()
   TRY
   {
     if (i_index.isEmpty())
     {
       i_index.resize(ni);
       for(int idx = 0; idx < ni; ++idx) i_index(idx)=ibegin+idx;
     }

     if (!lonvalue_unstructured_read_from_file.isEmpty() && lonvalue_1d.isEmpty())
     {
        lonvalue_1d.resize(ni);
        for (int idx = 0; idx < ni; ++idx)
          lonvalue_1d(idx) = lonvalue_unstructured_read_from_file(idx);

        // We dont need these values anymore, so just delete them
        lonvalue_unstructured_read_from_file.free();
     } 

     if (!latvalue_unstructured_read_from_file.isEmpty() && latvalue_1d.isEmpty())
     {
        latvalue_1d.resize(ni);
        for (int idx = 0; idx < ni; ++idx)
          latvalue_1d(idx) =  latvalue_unstructured_read_from_file(idx);

        // We dont need these values anymore, so just delete them
        latvalue_unstructured_read_from_file.free();
     }

     if (!bounds_lonvalue_unstructured_read_from_file.isEmpty() && bounds_lon_1d.isEmpty())
     {
        int nbVertex = nvertex;
        bounds_lon_1d.resize(nbVertex,ni);
        for (int idx = 0; idx < ni; ++idx)
          for (int jdx = 0; jdx < nbVertex; ++jdx)
            bounds_lon_1d(jdx,idx) = bounds_lonvalue_unstructured_read_from_file(jdx, idx);

        // We dont need these values anymore, so just delete them
        bounds_lonvalue_unstructured_read_from_file.free();
     }

     if (!bounds_latvalue_unstructured_read_from_file.isEmpty() && bounds_lat_1d.isEmpty())
     {
        int nbVertex = nvertex;
        bounds_lat_1d.resize(nbVertex,ni);
        for (int idx = 0; idx < ni; ++idx)
          for (int jdx = 0; jdx < nbVertex; ++jdx)
            bounds_lat_1d(jdx,idx) = bounds_latvalue_unstructured_read_from_file(jdx, idx);

        // We dont need these values anymore, so just delete them
        bounds_latvalue_unstructured_read_from_file.free();
     }
   }
   CATCH_DUMP_ATTR

  /*
    Get global longitude and latitude of rectilinear domain.
  */
   void CDomain::AllgatherRectilinearLonLat(CArray<double,1>& lon, CArray<double,1>& lat, CArray<double,1>& lon_g, CArray<double,1>& lat_g)
   TRY
   {
     CContext* context = CContext::getCurrent();
    // For now the assumption is that secondary server pools consist of the same number of procs.
    // CHANGE the line below if the assumption changes.
     int clientSize = context->intraCommSize_ ;
     lon_g.resize(ni_glo) ;
     lat_g.resize(nj_glo) ;


     int* ibegin_g = new int[clientSize] ;
     int* jbegin_g = new int[clientSize] ;
     int* ni_g = new int[clientSize] ;
     int* nj_g = new int[clientSize] ;
     int v ;
     v=ibegin ;
     MPI_Allgather(&v,1,MPI_INT,ibegin_g,1,MPI_INT,context->intraComm_) ;
     v=jbegin ;
     MPI_Allgather(&v,1,MPI_INT,jbegin_g,1,MPI_INT,context->intraComm_) ;
     v=ni ;
     MPI_Allgather(&v,1,MPI_INT,ni_g,1,MPI_INT,context->intraComm_) ;
     v=nj ;
     MPI_Allgather(&v,1,MPI_INT,nj_g,1,MPI_INT,context->intraComm_) ;

     MPI_Allgatherv(lon.dataFirst(),ni,MPI_DOUBLE,lon_g.dataFirst(),ni_g, ibegin_g,MPI_DOUBLE,context->intraComm_) ;
     MPI_Allgatherv(lat.dataFirst(),nj,MPI_DOUBLE,lat_g.dataFirst(),nj_g, jbegin_g,MPI_DOUBLE,context->intraComm_) ;

      delete[] ibegin_g ;
      delete[] jbegin_g ;
      delete[] ni_g ;
      delete[] nj_g ;
   }
   CATCH_DUMP_ATTR

   void CDomain::fillInRectilinearBoundLonLat(CArray<double,1>& lon, CArray<double,1>& lat,
                                              CArray<double,2>& boundsLon, CArray<double,2>& boundsLat)
   TRY
  {
     int i,j,k;

     const int nvertexValue = 4;
     boundsLon.resize(nvertexValue,ni*nj);

     if (ni_glo>1)
     {
       double lonStepStart = lon(1)-lon(0);
       bounds_lon_start=lon(0) - lonStepStart/2;
       double lonStepEnd = lon(ni_glo-1)-lon(ni_glo-2);
       bounds_lon_end=lon(ni_glo-1) + lonStepEnd/2;
       double errorBoundsLon = std::abs(360-std::abs(bounds_lon_end-bounds_lon_start));

       // if errorBoundsLon is reasonably small (0.1 x cell size) consider it as closed in longitude
       if (errorBoundsLon < std::abs(lonStepStart)*1e-1 || errorBoundsLon < std::abs(lonStepEnd)*1e-1 )
       {
         bounds_lon_start= (lon(0) + lon(ni_glo-1)-360)/2 ;
         bounds_lon_end= (lon(0) +360 + lon(ni_glo-1))/2 ;
       }
     }
     else
     {
       if (bounds_lon_start.isEmpty()) bounds_lon_start=-180. ;
       if (bounds_lon_end.isEmpty()) bounds_lon_end=180.-1e-8 ;
     }

     for(j=0;j<nj;++j)
       for(i=0;i<ni;++i)
       {
         k=j*ni+i;
         boundsLon(0,k) = boundsLon(1,k) = (0 == (ibegin + i)) ? bounds_lon_start
                                                               : (lon(ibegin + i)+lon(ibegin + i-1))/2;
         boundsLon(2,k) = boundsLon(3,k) = ((ibegin + i + 1) == ni_glo) ? bounds_lon_end
                                                                        : (lon(ibegin + i + 1)+lon(ibegin + i))/2;
       }


    boundsLat.resize(nvertexValue,nj*ni);
    bool isNorthPole=false ;
    bool isSouthPole=false ;
    if (std::abs(90 - std::abs(lat(0))) < NumTraits<double>::epsilon()) isNorthPole = true;
    if (std::abs(90 - std::abs(lat(nj_glo-1))) < NumTraits<double>::epsilon()) isSouthPole = true;

    // lat boundaries beyond pole the assimilate it to pole
    // lat boundarie is relativelly close to pole (0.1 x cell size) assimilate it to pole
    if (nj_glo>1)
    {
      double latStepStart = lat(1)-lat(0);
      if (isNorthPole) bounds_lat_start=lat(0);
      else
      {
        bounds_lat_start=lat(0)-latStepStart/2;
        if (bounds_lat_start >= 90 ) bounds_lat_start=90 ;
        else if (bounds_lat_start <= -90 ) bounds_lat_start=-90 ;
        else if (bounds_lat_start <= 90 && bounds_lat_start >= lat(0))
        {
          if ( std::abs(90-bounds_lat_start) <= 0.1*std::abs(latStepStart)) bounds_lat_start=90 ;
        }
        else if (bounds_lat_start >= -90 && bounds_lat_start <= lat(0))
        {
          if ( std::abs(-90 - bounds_lat_start) <= 0.1*std::abs(latStepStart)) bounds_lat_start=-90 ;
        }
      }

      double latStepEnd = lat(nj_glo-1)-lat(nj_glo-2);
      if (isSouthPole) bounds_lat_end=lat(nj_glo-1);
      else
      {
        bounds_lat_end=lat(nj_glo-1)+latStepEnd/2;

        if (bounds_lat_end >= 90 ) bounds_lat_end=90 ;
        else if (bounds_lat_end <= -90 ) bounds_lat_end=-90 ;
        else if (bounds_lat_end <= 90 && bounds_lat_end >= lat(nj_glo-1))
        {
          if ( std::abs(90-bounds_lat_end) <= 0.1*std::abs(latStepEnd)) bounds_lat_end=90 ;
        }
        else if (bounds_lat_end >= -90 && bounds_lat_end <= lat(nj_glo-1))
        {
          if ( std::abs(-90 - bounds_lat_end) <= 0.1*std::abs(latStepEnd)) bounds_lat_end=-90 ;
        }
      }
    }
    else
    {
      if (bounds_lat_start.isEmpty()) bounds_lat_start=-90. ;
      if (bounds_lat_end.isEmpty()) bounds_lat_end=90 ;
    }

    for(j=0;j<nj;++j)
      for(i=0;i<ni;++i)
      {
        k=j*ni+i;
        boundsLat(1,k) = boundsLat(2,k) = (0 == (jbegin + j)) ? bounds_lat_start
                                                              : (lat(jbegin + j)+lat(jbegin + j-1))/2;
        boundsLat(0,k) = boundsLat(3,k) = ((jbegin + j +1) == nj_glo) ? bounds_lat_end
                                                                      : (lat(jbegin + j + 1)+lat(jbegin + j))/2;
      }
   }
   CATCH_DUMP_ATTR

   /*
     General check of the domain to verify its mandatory attributes
   */
   void CDomain::checkDomain(void)
   TRY
   {
     if (type.isEmpty())
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The domain type is mandatory, "
             << "please define the 'type' attribute.")
     }

     if (type == type_attr::gaussian) 
     {
        hasPole=true ;
        type.setValue(type_attr::unstructured) ;
      }
      else if (type == type_attr::rectilinear) hasPole=true ;

     if (type == type_attr::unstructured)
     {
        if (ni_glo.isEmpty())
        {
          ERROR("CDomain::checkDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The global domain is badly defined, "
                << "the mandatory 'ni_glo' attribute is missing.")
        }
        else if (ni_glo <= 0)
        {
          ERROR("CDomain::checkDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The global domain is badly defined, "
                << "'ni_glo' attribute should be strictly positive so 'ni_glo = " << ni_glo.getValue() << "' is invalid.")
        }
        isUnstructed_ = true;
        nj_glo = 1;
        nj = 1;
        jbegin = 0;
        if (!i_index.isEmpty()) 
        {
          ni = i_index.numElements();
          j_index.resize(ni);
          for(int i=0;i<ni;++i) j_index(i)=0;
        }
        

        if (!area.isEmpty()) area.transposeSelf(1, 0); // => to be checked why is it transposed
     }

     if (ni_glo.isEmpty())
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "the mandatory 'ni_glo' attribute is missing.")
     }
     else if (ni_glo <= 0)
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "'ni_glo' attribute should be strictly positive so 'ni_glo = " << ni_glo.getValue() << "' is invalid.")
     }

     if (nj_glo.isEmpty())
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "the mandatory 'nj_glo' attribute is missing.")
     }
     else if (nj_glo <= 0)
     {
       ERROR("CDomain::checkDomain(void)",
             << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
             << "The global domain is badly defined, "
             << "'nj_glo' attribute should be strictly positive so 'nj_glo = " << nj_glo.getValue() << "' is invalid.")
     }

     checkLocalIDomain();
     checkLocalJDomain();

     if (i_index.isEmpty())
     {
       i_index.resize(ni*nj);
       for (int j = 0; j < nj; ++j)
         for (int i = 0; i < ni; ++i) i_index(i+j*ni) = i+ibegin;
     }

     if (j_index.isEmpty())
     {
       j_index.resize(ni*nj);
       for (int j = 0; j < nj; ++j)
         for (int i = 0; i < ni; ++i) j_index(i+j*ni) = j+jbegin;
     }
   }
   CATCH_DUMP_ATTR

   size_t CDomain::getGlobalWrittenSize(void)
   {
     return ni_glo*nj_glo ;
   }
   //----------------------------------------------------------------

   // Check validity of local domain on using the combination of 3 parameters: ibegin, ni and i_index
   void CDomain::checkLocalIDomain(void)
   TRY
   {
      // If ibegin and ni are provided then we use them to check the validity of local domain
      if (i_index.isEmpty() && !ibegin.isEmpty() && !ni.isEmpty())
      {
        if ((ni.getValue() < 0 || ibegin.getValue() < 0) || ((ibegin.getValue() + ni.getValue()) > ni_glo.getValue()))
        {
          ERROR("CDomain::checkLocalIDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The local domain is wrongly defined,"
                << " check the attributes 'ni_glo' (" << ni_glo.getValue() << "), 'ni' (" << ni.getValue() << ") and 'ibegin' (" << ibegin.getValue() << ")");
        }
      }

      // i_index has higher priority than ibegin and ni
      if (!i_index.isEmpty())
      {
        int minIIndex = (0 < i_index.numElements()) ? i_index(0) : 0;
        if (ni.isEmpty()) 
        {          
         // No information about ni
          int minIndex = ni_glo - 1;
          int maxIndex = 0;
          for (int idx = 0; idx < i_index.numElements(); ++idx)
          {
            if (i_index(idx) < minIndex) minIndex = i_index(idx);
            if (i_index(idx) > maxIndex) maxIndex = i_index(idx);
          }
	  if (i_index.numElements()) {
            ni = maxIndex - minIndex + 1; 
            minIIndex = minIndex;
          }	    
	  else {
            ni = 0;
	  }
        }

        // It's not so correct but if ibegin is not the first value of i_index 
        // then data on local domain has user-defined distribution. In this case, ibegin, ni have no meaning.
        if (ibegin.isEmpty()) ibegin = minIIndex;
      }
      else if (ibegin.isEmpty() && ni.isEmpty())
      {
        ibegin = 0;
        ni = ni_glo;
      }
      else if ((!ibegin.isEmpty() && ni.isEmpty()) || (ibegin.isEmpty() && !ni.isEmpty()))
      {
        ERROR("CDomain::checkLocalIDomain(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The local domain is wrongly defined," << endl
              << "i_index is empty and either 'ni' or 'ibegin' is not defined. " 
              << "If 'ni' and 'ibegin' are used to define a domain, both of them must not be empty.");
      }
       

      if ((ni.getValue() < 0 || ibegin.getValue() < 0))
      {
        ERROR("CDomain::checkLocalIDomain(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The local domain is wrongly defined,"
              << " check the attributes 'ni_glo' (" << ni_glo.getValue() << "), 'ni' (" << ni.getValue() << ") and 'ibegin' (" << ibegin.getValue() << ")");
      }
   }
   CATCH_DUMP_ATTR

   // Check validity of local domain on using the combination of 3 parameters: jbegin, nj and j_index
   void CDomain::checkLocalJDomain(void)
   TRY
   {
    // If jbegin and nj are provided then we use them to check the validity of local domain
     if (j_index.isEmpty() && !jbegin.isEmpty() && !nj.isEmpty())
     {
       if ((nj.getValue() < 0 || jbegin.getValue() < 0) || (jbegin.getValue() + nj.getValue()) > nj_glo.getValue())
       {
         ERROR("CDomain::checkLocalJDomain(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The local domain is wrongly defined,"
                << " check the attributes 'nj_glo' (" << nj_glo.getValue() << "), 'nj' (" << nj.getValue() << ") and 'jbegin' (" << jbegin.getValue() << ")");
       }
     }

     if (!j_index.isEmpty())
     {
        int minJIndex = (0 < j_index.numElements()) ? j_index(0) : 0;
        if (nj.isEmpty()) 
        {
          // No information about nj
          int minIndex = nj_glo - 1;
          int maxIndex = 0;
          for (int idx = 0; idx < j_index.numElements(); ++idx)
          {
            if (j_index(idx) < minIndex) minIndex = j_index(idx);
            if (j_index(idx) > maxIndex) maxIndex = j_index(idx);
          }
	  if (j_index.numElements()) {
            nj = maxIndex - minIndex + 1;
            minJIndex = minIndex; 
	  }
          else
            nj = 0;
        }  
        // It's the same as checkLocalIDomain. It's not so correct but if jbegin is not the first value of j_index 
        // then data on local domain has user-defined distribution. In this case, jbegin has no meaning.
       if (jbegin.isEmpty()) jbegin = minJIndex;       
     }
     else if (jbegin.isEmpty() && nj.isEmpty())
     {
       jbegin = 0;
       nj = nj_glo;
     }      


     if ((nj.getValue() < 0 || jbegin.getValue() < 0))
     {
       ERROR("CDomain::checkLocalJDomain(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The local domain is wrongly defined,"
              << " check the attributes 'nj_glo' (" << nj_glo.getValue() << "), 'nj' (" << nj.getValue() << ") and 'jbegin' (" << jbegin.getValue() << ")");
     }
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   void CDomain::checkMask(void)
   TRY
   {
      if (!mask_1d.isEmpty() && !mask_2d.isEmpty())
        ERROR("CDomain::checkMask(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "Both mask_1d and mask_2d are defined but only one can be used at the same time." << std::endl
              << "Please define only one mask: 'mask_1d' or 'mask_2d'.");

      if (!mask_1d.isEmpty() && mask_2d.isEmpty())
      {
        if (mask_1d.numElements() != i_index.numElements())
          ERROR("CDomain::checkMask(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "'mask_1d' does not have the same size as the local domain." << std::endl
                << "Local size is " << i_index.numElements() << "." << std::endl
                << "Mask size is " << mask_1d.numElements() << ".");
      }

      if (mask_1d.isEmpty() && !mask_2d.isEmpty())
      {
        if (mask_2d.extent(0) != ni || mask_2d.extent(1) != nj)
          ERROR("CDomain::checkMask(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "The mask does not have the same size as the local domain." << std::endl
                << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
                << "Mask size is " << mask_2d.extent(0) << " x " << mask_2d.extent(1) << ".");
      }

      if (!mask_2d.isEmpty())
      {
        domainMask.resize(mask_2d.extent(0) * mask_2d.extent(1));
        for (int j = 0; j < nj; ++j)
          for (int i = 0; i < ni; ++i) domainMask(i+j*ni) = mask_2d(i,j);
//        mask_2d.reset();
      }
      else if (mask_1d.isEmpty())
      {
        domainMask.resize(i_index.numElements());
        for (int i = 0; i < i_index.numElements(); ++i) domainMask(i) = true;
      }
      else
      {
      domainMask.resize(mask_1d.numElements());
      domainMask=mask_1d ;
     }
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   void CDomain::checkDomainData(void)
   TRY
   {
      if (data_dim.isEmpty())
      {
        data_dim.setValue(1);
      }
      else if (!(data_dim.getValue() == 1 || data_dim.getValue() == 2))
      {
        ERROR("CDomain::checkDomainData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data dimension is invalid, 'data_dim' must be 1 or 2 not << " << data_dim.getValue() << ".");
      }

      if (data_ibegin.isEmpty())
         data_ibegin.setValue(0);
      if (data_jbegin.isEmpty())
         data_jbegin.setValue(0);

      if (data_ni.isEmpty())
      {
        data_ni.setValue((data_dim == 1) ? (ni.getValue() * nj.getValue()) : ni.getValue());
      }
      else if (data_ni.getValue() < 0)
      {
        ERROR("CDomain::checkDomainData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data size cannot be negative ('data_ni' = " << data_ni.getValue() << ").");
      }

      if (data_nj.isEmpty())
      {
        data_nj.setValue((data_dim.getValue() == 1) ? (ni.getValue() * nj.getValue()) : nj.getValue());
      }
      else if (data_nj.getValue() < 0)
      {
        ERROR("CDomain::checkDomainData(void)",
              << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
              << "The data size cannot be negative ('data_nj' = " << data_nj.getValue() << ").");
      }
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------

   void CDomain::checkCompression(void)
   TRY
   {
     int i,j,ind;
      if (!data_i_index.isEmpty())
      {
        if (!data_j_index.isEmpty() &&
            data_j_index.numElements() != data_i_index.numElements())
        {
           ERROR("CDomain::checkCompression(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'data_i_index' and 'data_j_index' arrays must have the same size." << std::endl
                 << "'data_i_index' size = " << data_i_index.numElements() << std::endl
                 << "'data_j_index' size = " << data_j_index.numElements());
        }

        if (2 == data_dim)
        {
          if (data_j_index.isEmpty())
          {
             ERROR("CDomain::checkCompression(void)",
                   << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                   << "'data_j_index' must be defined when 'data_i_index' is set and 'data_dim' is 2.");
          }
          for (int k=0; k<data_i_index.numElements(); ++k)
          {
            i = data_i_index(k)+data_ibegin ;
            j = data_j_index(k)+data_jbegin ;
            if (i>=0 && i<ni && j>=0 && j<nj)
            {
              ind=j*ni+i ;
              if ( (ind<0)||(!domainMask(ind)) )
              {
                data_i_index(k) = -1;
                data_j_index(k) = -1;
              }
            }
            else
            {
              data_i_index(k) = -1;
              data_j_index(k) = -1;
            }
          }
        }
        else // (1 == data_dim)
        {
          if (data_j_index.isEmpty())
          {
            data_j_index.resize(data_ni);
            data_j_index = 0;
          }
          for (int k=0; k<data_i_index.numElements(); ++k)
          {
            i=data_i_index(k)+data_ibegin ;
            if (i>=0 && i < domainMask.size())
            {
              if (!domainMask(i)) data_i_index(k) = -1;
            }
            else
              data_i_index(k) = -1;

            if ( (i<0)||(!domainMask(i)) ) data_i_index(k) = -1;
          }
        }
      }
      else
      {
        if (data_dim == 2 && !data_j_index.isEmpty())
          ERROR("CDomain::checkCompression(void)",
                << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                << "'data_i_index' must be defined when 'data_j_index' is set and 'data_dim' is 2.");

        if (1 == data_dim)
        {
          data_i_index.resize(data_ni);
          data_j_index.resize(data_ni);
          data_j_index = 0;

          for (int k = 0; k < data_ni; ++k)
          {
            i=k+data_ibegin ;
            if (i>=0 && i < domainMask.size())
            {
              if ( (i<0)||(!domainMask(i)) )
                data_i_index(k) = -1;
              else
                data_i_index(k) = k;
            }
            else
              data_i_index(k) = -1;
          }
        }
        else // (data_dim == 2)
        {
          const int dsize = data_ni * data_nj;
          data_i_index.resize(dsize);
          data_j_index.resize(dsize);

          for(int count = 0, kj = 0; kj < data_nj; ++kj)
          {
            for(int ki = 0; ki < data_ni; ++ki, ++count)
            {
              i = ki + data_ibegin;
              j = kj + data_jbegin;
              ind=j*ni+i ;
              if (i>=0 && i<ni && j>=0 && j<nj)
              {
                if ( (ind<0)||(!domainMask(ind)) )
                {
                  data_i_index(count) = -1;
                  data_j_index(count) = -1;
                }
                else
                {
                  data_i_index(count) = ki;
                  data_j_index(count) = kj;
                }
              }
              else
              {
                data_i_index(count) = -1;
                data_j_index(count) = -1;
              }
            }
          }
        }
      }
   }
   CATCH_DUMP_ATTR

   //----------------------------------------------------------------
   void CDomain::computeLocalMask(void)
   TRY
   {
     localMask.resize(i_index.numElements()) ;
     localMask=false ;

     size_t dn=data_i_index.numElements() ;
     int i,j ;
     size_t k,ind ;

     for(k=0;k<dn;k++)
     {
       if (data_dim==2)
       {
          i=data_i_index(k)+data_ibegin ;
          j=data_j_index(k)+data_jbegin ;
          if (i>=0 && i<ni && j>=0 && j<nj)
          {
            ind=j*ni+i ;
            localMask(ind)=domainMask(ind) ;
          }
       }
       else
       {
          i=data_i_index(k)+data_ibegin ;
          if (i>=0 && i<i_index.numElements())
          {
            ind=i ;
            localMask(ind)=domainMask(ind) ;
          }
       }
     }
   }
   CATCH_DUMP_ATTR


   //----------------------------------------------------------------

   /*
     Fill in longitude, latitude, bounds, and area into internal values (lonvalue, latvalue, bounds_lonvalue, bounds_latvalue, areavalue)
     which will be used by XIOS.
   */
   void CDomain::completeLonLatClient(void)
   TRY
   {
     bool lonlatValueExisted = (0 != lonvalue.numElements()) || (0 != latvalue.numElements());
     checkBounds() ;
     checkArea() ;

     if (!lonvalue_2d.isEmpty() && !lonlatValueExisted)
     {
       lonvalue.resize(ni * nj);
       latvalue.resize(ni * nj);
       if (hasBounds)
       {
         bounds_lonvalue.resize(nvertex, ni * nj);
         bounds_latvalue.resize(nvertex, ni * nj);
       }

       for (int j = 0; j < nj; ++j)
       {
         for (int i = 0; i < ni; ++i)
         {
           int k = j * ni + i;

           lonvalue(k) = lonvalue_2d(i,j);
           latvalue(k) = latvalue_2d(i,j);

           if (hasBounds)
           {
             for (int n = 0; n < nvertex; ++n)
             {
               bounds_lonvalue(n,k) = bounds_lon_2d(n,i,j);
               bounds_latvalue(n,k) = bounds_lat_2d(n,i,j);
             }
           }
         }
       }
     }
     else if (!lonvalue_1d.isEmpty()  && !lonlatValueExisted)
     {
       if (type_attr::rectilinear == type)
       {
         if (ni == lonvalue_1d.numElements() && nj == latvalue_1d.numElements())
         {
           lonvalue.resize(ni * nj);
           latvalue.resize(ni * nj);
           if (hasBounds)
           {
             bounds_lonvalue.resize(nvertex, ni * nj);
             bounds_latvalue.resize(nvertex, ni * nj);
           }

           for (int j = 0; j < nj; ++j)
           {
             for (int i = 0; i < ni; ++i)
             {
               int k = j * ni + i;

               lonvalue(k) = lonvalue_1d(i);
               latvalue(k) = latvalue_1d(j);

               if (hasBounds)
               {
                 for (int n = 0; n < nvertex; ++n)
                 {
                   bounds_lonvalue(n,k) = bounds_lon_1d(n,i);
                   bounds_latvalue(n,k) = bounds_lat_1d(n,j);
                 }
               }
             }
           }
         }
         else if (i_index.numElements() == lonvalue_1d.numElements() && j_index.numElements() == latvalue_1d.numElements()  && !lonlatValueExisted)
         {
           lonvalue.reference(lonvalue_1d);
           latvalue.reference(latvalue_1d);
            if (hasBounds)
           {
             bounds_lonvalue.reference(bounds_lon_1d);
             bounds_latvalue.reference(bounds_lat_1d);
           }
         }
         else
           ERROR("CDomain::completeLonClient(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_1d' and 'latvalue_1d' does not have the same size as the local domain." << std::endl
                 << "'lonvalue_1d' size is " << lonvalue_1d.numElements() 
                 << " and 'latvalue_1d' size is " << latvalue_1d.numElements() << std::endl 
                 << " They should be correspondingly " << ni.getValue() << " and "  << nj.getValue() << " or " << std::endl
                 << i_index.numElements() << " and "  << j_index.numElements() << ".");
       }
       else if (type == type_attr::curvilinear || type == type_attr::unstructured  && !lonlatValueExisted)
       {
         lonvalue.reference(lonvalue_1d);
         latvalue.reference(latvalue_1d);
         if (hasBounds)
         {
           bounds_lonvalue.reference(bounds_lon_1d);
           bounds_latvalue.reference(bounds_lat_1d);
         }
       }
     }

     if (!area.isEmpty() && areavalue.isEmpty())
     {
        areavalue.resize(ni*nj);
       for (int j = 0; j < nj; ++j)
       {
         for (int i = 0; i < ni; ++i)
         {
           int k = j * ni + i;
           areavalue(k) = area(i,j);
         }
       }
     }
   }
   CATCH_DUMP_ATTR

   /*
     Convert internal longitude latitude value used by XIOS to "lonvalue_*" which can be retrieved with Fortran interface
   */
   void CDomain::convertLonLatValue(void)
   TRY
   {
     bool lonlatValueExisted = (0 != lonvalue.numElements()) || (0 != latvalue.numElements());
     if (!lonvalue_2d.isEmpty() && lonlatValueExisted)
     {
       lonvalue_2d.resize(ni,nj);
       latvalue_2d.resize(ni,nj);
       if (hasBounds)
       {
         bounds_lon_2d.resize(nvertex, ni, nj);
         bounds_lat_2d.resize(nvertex, ni, nj);
       }

       for (int j = 0; j < nj; ++j)
       {
         for (int i = 0; i < ni; ++i)
         {
           int k = j * ni + i;

           lonvalue_2d(i,j) = lonvalue(k);
           latvalue_2d(i,j) = latvalue(k);

           if (hasBounds)
           {
             for (int n = 0; n < nvertex; ++n)
             {
               bounds_lon_2d(n,i,j) = bounds_lonvalue(n,k);
               bounds_lat_2d(n,i,j) = bounds_latvalue(n,k);
             }
           }
         }
       }
     }
     else if (!lonvalue_1d.isEmpty()  && lonlatValueExisted)
     {
       if (type_attr::rectilinear == type)
       {
         if (ni == lonvalue_1d.numElements() && nj == latvalue_1d.numElements())
         {
           lonvalue.resize(ni * nj);
           latvalue.resize(ni * nj);
           if (hasBounds)
           {
             bounds_lonvalue.resize(nvertex, ni * nj);
             bounds_latvalue.resize(nvertex, ni * nj);
           }

           for (int j = 0; j < nj; ++j)
           {
             for (int i = 0; i < ni; ++i)
             {
               int k = j * ni + i;

               lonvalue(k) = lonvalue_1d(i);
               latvalue(k) = latvalue_1d(j);

               if (hasBounds)
               {
                 for (int n = 0; n < nvertex; ++n)
                 {
                   bounds_lonvalue(n,k) = bounds_lon_1d(n,i);
                   bounds_latvalue(n,k) = bounds_lat_1d(n,j);
                 }
               }
             }
           }
         }
         else if (i_index.numElements() == lonvalue_1d.numElements() && j_index.numElements() == latvalue_1d.numElements()  && !lonlatValueExisted)
         {
           lonvalue.reference(lonvalue_1d);
           latvalue.reference(latvalue_1d);
            if (hasBounds)
           {
             bounds_lonvalue.reference(bounds_lon_1d);
             bounds_latvalue.reference(bounds_lat_1d);
           }
         }
         else
           ERROR("CDomain::completeLonClient(void)",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_1d' and 'latvalue_1d' does not have the same size as the local domain." << std::endl
                 << "'lonvalue_1d' size is " << lonvalue_1d.numElements() 
                 << " and 'latvalue_1d' size is " << latvalue_1d.numElements() << std::endl 
                 << " They should be correspondingly " << ni.getValue() << " and "  << nj.getValue() << " or " << std::endl
                 << i_index.numElements() << " and "  << j_index.numElements() << ".");
       }
       else if (type == type_attr::curvilinear || type == type_attr::unstructured  && !lonlatValueExisted)
       {
         lonvalue.reference(lonvalue_1d);
         latvalue.reference(latvalue_1d);
         if (hasBounds)
         {
           bounds_lonvalue.reference(bounds_lon_1d);
           bounds_latvalue.reference(bounds_lat_1d);
         }
       }
     }
   }
   CATCH_DUMP_ATTR

   void CDomain::checkBounds(void)
   TRY
   {
     bool hasBoundValues = (0 != bounds_lonvalue.numElements()) || (0 != bounds_latvalue.numElements());
     if (!nvertex.isEmpty() && nvertex > 0 && !hasBoundValues)
     {
       if (!bounds_lon_1d.isEmpty() && !bounds_lon_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one longitude boundary attribute can be used but both 'bounds_lon_1d' and 'bounds_lon_2d' are defined." << std::endl
               << "Define only one longitude boundary attribute: 'bounds_lon_1d' or 'bounds_lon_2d'.");

       if (!bounds_lat_1d.isEmpty() && !bounds_lat_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one latitude boundary attribute can be used but both 'bounds_lat_1d' and 'bounds_lat_2d' are defined." << std::endl
               << "Define only one latitude boundary attribute: 'bounds_lat_1d' or 'bounds_lat_2d'.");

       if ((!bounds_lon_1d.isEmpty() && bounds_lat_1d.isEmpty()) || (bounds_lon_1d.isEmpty() && !bounds_lat_1d.isEmpty()))
       {
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only 'bounds_lon_1d' or 'bounds_lat_1d' is defined." << std::endl
               << "Please define either both attributes or none.");
       }

       if ((!bounds_lon_2d.isEmpty() && bounds_lat_2d.isEmpty()) || (bounds_lon_2d.isEmpty() && !bounds_lat_2d.isEmpty()))
       {
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only 'bounds_lon_2d' or 'bounds_lat_2d' is defined." << std::endl
               << "Please define either both attributes or none.");
       }

       if (!bounds_lon_1d.isEmpty() && nvertex.getValue() != bounds_lon_1d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lon_1d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lon_1d' dimension is " << bounds_lon_1d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lon_2d.isEmpty() && nvertex.getValue() != bounds_lon_2d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lon_2d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lon_2d' dimension is " << bounds_lon_2d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lon_1d.isEmpty() && lonvalue_1d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lon_1d' is defined, 'lonvalue_1d' must be defined too." << std::endl);

       if (!bounds_lon_2d.isEmpty() && lonvalue_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lon_2d' is defined, 'lonvalue_2d' must be defined too." << std::endl);

       if (!bounds_lat_1d.isEmpty() && nvertex.getValue() != bounds_lat_1d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lat_1d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lat_1d' dimension is " << bounds_lat_1d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lat_2d.isEmpty() && nvertex.getValue() != bounds_lat_2d.extent(0))
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "'bounds_lat_2d' dimension is not compatible with 'nvertex'." << std::endl
               << "'bounds_lat_2d' dimension is " << bounds_lat_2d.extent(0)
               << " but nvertex is " << nvertex.getValue() << ".");

       if (!bounds_lat_1d.isEmpty() && latvalue_1d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Since 'bounds_lat_1d' is defined, 'latvalue_1d' must be defined too." << std::endl);

       if (!bounds_lat_2d.isEmpty() && latvalue_2d.isEmpty())
         ERROR("CDomain::checkBounds(void)",
               << "Since 'bounds_lat_2d' is defined, 'latvalue_2d' must be defined too." << std::endl);

       // In case of reading UGRID bounds values are not required
       hasBounds = (!bounds_lat_1d.isEmpty() || !bounds_lat_2d.isEmpty() );
     }
     else if (hasBoundValues)
     {
       hasBounds = true;       
     }
     else
     {
       hasBounds = false;
     }
   }
   CATCH_DUMP_ATTR

   void CDomain::checkArea(void)
   TRY
   {
     bool hasAreaValue = (!areavalue.isEmpty() && 0 != areavalue.numElements());
     hasArea = !area.isEmpty();
     if (hasArea && !hasAreaValue)
     {
       if (area.extent(0) != ni || area.extent(1) != nj)
       {
         ERROR("CDomain::checkArea(void)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "The area does not have the same size as the local domain." << std::endl
               << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
               << "Area size is " << area.extent(0) << " x " << area.extent(1) << ".");
       }
//       if (areavalue.isEmpty())
//       {
//          areavalue.resize(ni*nj);
//         for (int j = 0; j < nj; ++j)
//         {
//           for (int i = 0; i < ni; ++i)
//           {
//             int k = j * ni + i;
//             areavalue(k) = area(i,j);
//           }
//         }
//       }
     }
   }
   CATCH_DUMP_ATTR

   void CDomain::checkLonLat()
   TRY
   {
     if (!hasLonLat) hasLonLat = (!latvalue_1d.isEmpty() && !lonvalue_1d.isEmpty()) ||
                                 (!latvalue_2d.isEmpty() && !lonvalue_2d.isEmpty());
     bool hasLonLatValue = (0 != lonvalue.numElements()) || (0 != latvalue.numElements());
     if (hasLonLat && !hasLonLatValue)
     {
       if (!lonvalue_1d.isEmpty() && !lonvalue_2d.isEmpty())
         ERROR("CDomain::checkLonLat()",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one longitude attribute can be used but both 'lonvalue_1d' and 'lonvalue_2d' are defined." << std::endl
               << "Define only one longitude attribute: 'lonvalue_1d' or 'lonvalue_2d'.");

       if (!lonvalue_1d.isEmpty() && lonvalue_2d.isEmpty())
       {
         if ((type_attr::rectilinear != type) && (lonvalue_1d.numElements() != i_index.numElements()))
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_1d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << i_index.numElements() << "." << std::endl
                 << "'lonvalue_1d' size is " << lonvalue_1d.numElements() << ".");
       }

       if (lonvalue_1d.isEmpty() && !lonvalue_2d.isEmpty())
       {
         if (lonvalue_2d.extent(0) != ni || lonvalue_2d.extent(1) != nj)
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'lonvalue_2d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
                 << "'lonvalue_2d' size is " << lonvalue_2d.extent(0) << " x " << lonvalue_2d.extent(1) << ".");
       }

       if (!latvalue_1d.isEmpty() && !latvalue_2d.isEmpty())
         ERROR("CDomain::checkLonLat()",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one latitude attribute can be used but both 'latvalue_1d' and 'latvalue_2d' are defined." << std::endl
               << "Define only one latitude attribute: 'latvalue_1d' or 'latvalue_2d'.");

       if (!latvalue_1d.isEmpty() && latvalue_2d.isEmpty())
       {
         if ((type_attr::rectilinear != type) && (latvalue_1d.numElements() != i_index.numElements()))
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'latvalue_1d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << i_index.numElements() << "." << std::endl
                 << "'latvalue_1d' size is " << latvalue_1d.numElements() << ".");
       }

       if (latvalue_1d.isEmpty() && !latvalue_2d.isEmpty())
       {
         if (latvalue_2d.extent(0) != ni || latvalue_2d.extent(1) != nj)
           ERROR("CDomain::checkLonLat()",
                 << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
                 << "'latvalue_2d' does not have the same size as the local domain." << std::endl
                 << "Local size is " << ni.getValue() << " x " << nj.getValue() << "." << std::endl
                 << "'latvalue_2d' size is " << latvalue_2d.extent(0) << " x " << latvalue_2d.extent(1) << ".");
       }
     }
   }
   CATCH_DUMP_ATTR

   void CDomain::checkAttributes(void)
   TRY
   {
      if (this->checkAttributes_done_) return;
      this->checkDomain();
      this->checkLonLat();
      this->checkBounds();
      this->checkArea();
      this->checkMask();
      this->checkDomainData();
      this->checkCompression();
      this->computeLocalMask() ;
      this->completeLonLatClient();
      this->initializeLocalElement() ;
      this->addFullView() ; // probably do not automatically add View, but only if requested
      this->addWorkflowView() ; // probably do not automatically add View, but only if requested
      this->addModelView() ; // probably do not automatically add View, but only if requested
      // testing ?
     /*
      shared_ptr<CLocalView> local = localElement_->getView(CElementView::WORKFLOW) ;
      shared_ptr<CLocalView> model = localElement_->getView(CElementView::MODEL) ;

      CLocalConnector test1(model, local) ;
      test1.computeConnector() ;
      CLocalConnector test2(local, model) ;
      test2.computeConnector() ;
      CGridLocalConnector gridTest1(vector<CLocalConnector*>{&test1}) ;
      CGridLocalConnector gridTest2(vector<CLocalConnector*>{&test2}) ;
      
      
      CArray<int,1> out1 ;
      CArray<int,1> out2 ;
      test1.transfer(data_i_index,out1,-111) ;
      test2.transfer(out1,out2,-111) ;
      
      out1 = 0 ;
      out2 = 0 ;
      gridTest1.transfer(data_i_index,out1,-111) ;
      gridTest2.transfer(out1, out2,-111) ;
    */  
      this->checkAttributes_done_ = true;
   }
   CATCH_DUMP_ATTR


   void CDomain::initializeLocalElement(void)
   {
      // after checkDomain i_index and j_index of size (ni*nj) 
      int nij = ni*nj ;
      CArray<size_t, 1> ij_index(ni*nj) ;
      for(int ij=0; ij<nij ; ij++) ij_index(ij) = i_index(ij)+j_index(ij)*ni_glo ;
      int rank = CContext::getCurrent()->getIntraCommRank() ;
      localElement_ = make_shared<CLocalElement>(rank, ni_glo*nj_glo, ij_index) ;
   }

   void CDomain::addFullView(void)
   {
      CArray<int,1> index(ni*nj) ;
      int nij=ni*nj ;
      for(int ij=0; ij<nij ; ij++) index(ij)=ij ;
      localElement_ -> addView(CElementView::FULL, index) ;
   }

   void CDomain::addWorkflowView(void)
   {
     // information for workflow view is stored in localMask
     int nij=ni*nj ;
     int nMask=0 ;
     for(int ij=0; ij<nij ; ij++) if (localMask(ij)) nMask++ ;
     CArray<int,1> index(nMask) ;

     nMask=0 ;
     for(int ij=0; ij<nij ; ij++) 
      if (localMask(ij))
      {
        index(nMask)=ij ;
        nMask++ ;
      }
      localElement_ -> addView(CElementView::WORKFLOW, index) ;
   }

   void CDomain::addModelView(void)
   {
     // information for model view is stored in data_i_index/data_j_index
     // very weird, do not mix data_i_index and data_i_begin => in future only keep data_i_index
     int dataSize = data_i_index.numElements() ;
     CArray<int,1> index(dataSize) ;
     int i,j ;
     for(int k=0;k<dataSize;k++)
     {
        if (data_dim==2)
        {
          i=data_i_index(k)+data_ibegin ; // bad
          j=data_j_index(k)+data_jbegin ; // bad
          if (i>=0 && i<ni && j>=0 && j<nj) index(k)=i+j*ni ;
          else index(k)=-1 ;
        }
        else if (data_dim==1)
        {
          i=data_i_index(k)+data_ibegin ; // bad
          if (i>=0 && i<ni*nj) index(k)=i ;
          else index(k)=-1 ;
        }
     }
     localElement_->addView(CElementView::MODEL, index) ;
   }
        
   void CDomain::computeModelToWorkflowConnector(void)
   { 
     shared_ptr<CLocalView> srcView=getLocalView(CElementView::MODEL) ;
     shared_ptr<CLocalView> dstView=getLocalView(CElementView::WORKFLOW) ;
     modelToWorkflowConnector_ = make_shared<CLocalConnector>(srcView, dstView); 
     modelToWorkflowConnector_->computeConnector() ;
   }


  string CDomain::getCouplingAlias(const string& fieldId, int posInGrid)
  {
    return "_domain["+std::to_string(posInGrid)+"]_of_"+fieldId ;
  }
   
  /* to be removed later when coupling will be reimplemented, just to  not forget */
  void CDomain::sendDomainToCouplerOut(CContextClient* client, const string& fieldId, int posInGrid)
  {
    if (sendDomainToFileServer_done_.count(client)!=0) return ;
    else sendDomainToFileServer_done_.insert(client) ;
    
    const string domainId = getCouplingAlias(fieldId, posInGrid) ;
    
    if (!domain_ref.isEmpty())
    {
      auto domain_ref_tmp=domain_ref.getValue() ;
      domain_ref.reset() ; // remove the reference, find an other way to do that more cleanly
      this->sendAllAttributesToServer(client, domainId)  ;
      domain_ref = domain_ref_tmp ;
    }
    else this->sendAllAttributesToServer(client, domainId)  ;
  }




  void CDomain::makeAliasForCoupling(const string& fieldId, int posInGrid)
  {
    const string domainId = getCouplingAlias(fieldId, posInGrid);
    this->createAlias(domainId) ;
  }


  void CDomain::computeRemoteElement(CContextClient* client, EDistributionType distType)
  TRY
  {
    CContext* context = CContext::getCurrent();
    map<int, CArray<size_t,1>> globalIndex ;
/* old method
    if (type==EDistributionType::BANDS) // Bands distribution to send to file server
    {
      int nbServer = client->serverSize;
      std::vector<int> nGlobDomain(2);
      nGlobDomain[0] = this->ni_glo;
      nGlobDomain[1] = this->nj_glo;

      // to be changed in future, need to rewrite more simply domain distribution
      CServerDistributionDescription serverDescription(nGlobDomain, nbServer);
      int distributedPosition ;
      if (isUnstructed_) distributedPosition = 0 ;
      else distributedPosition = 1 ;
      
      std::vector<std::vector<int> > serverIndexBegin = serverDescription.getServerIndexBegin();
      std::vector<std::vector<int> > serverDimensionSizes = serverDescription.getServerDimensionSizes();
      vector<unordered_map<size_t,vector<int>>> indexServerOnElement ;
      CArray<int,1> axisDomainOrder(1) ; axisDomainOrder(0)=2 ;
      auto zeroIndex=serverDescription.computeServerGlobalByElement(indexServerOnElement, context->getIntraCommRank(), context->getIntraCommSize(),
                                                                  axisDomainOrder,distributedPosition) ;
      // distribution is very bad => to redo
      // convert indexServerOnElement => map<int,CArray<size_t,1>> - need to be changed later
      map<int, vector<size_t>> vectGlobalIndex ;
      for(auto& indexRanks : indexServerOnElement[0])
      {
        size_t index=indexRanks.first ;
        auto& ranks=indexRanks.second ;
        for(int rank : ranks) vectGlobalIndex[rank].push_back(index) ;
      }
      for(auto& vect : vectGlobalIndex ) globalIndex.emplace(vect.first, CArray<size_t,1>(vect.second.data(), shape(vect.second.size()),duplicateData)) ;
    // some servers receves no index (zeroIndex array) => root process take them into account.
      if (context->getIntraCommRank()==0) 
        for(auto& rank : zeroIndex) globalIndex[rank] = CArray<size_t,1>() ; 
    }
*/    
    if (distType==EDistributionType::BANDS && isUnstructed_) distType=EDistributionType::COLUMNS ;

    if (distType==EDistributionType::BANDS) // Bands distribution to send to file server
    {
      int nbServer = client->serverSize;
      int nbClient = client->clientSize ;
      int rankClient = client->clientRank ;
      int size = nbServer / nbClient ;
      int start ;
      if (nbServer%nbClient > rankClient)
      {
       start = (size+1) * rankClient ;
       size++ ;
      }
      else start = size*rankClient + nbServer%nbClient ;
     
      for(int i=0; i<size; i++)
      { 
        int rank=start+i ; 
        size_t indSize = nj_glo/nbServer ;
        size_t indStart ;
        if (nj_glo % nbServer > rank)
        {
          indStart = (indSize+1) * rank ;
          indSize++ ;
        }
        else indStart = indSize*rank + nj_glo%nbServer ;
       
        indStart=indStart*ni_glo ;
        indSize=indSize*ni_glo ;
        auto& globalInd =  globalIndex[rank] ;
        globalInd.resize(indSize) ;
        for(size_t n = 0 ; n<indSize; n++) globalInd(n)=indStart+n ;
      }
    }
    else if (distType==EDistributionType::COLUMNS) // Bands distribution to send to file server
    {
      int nbServer = client->serverSize;
      int nbClient = client->clientSize ;
      int rankClient = client->clientRank ;
      int size = nbServer / nbClient ;
      int start ;
      if (nbServer%nbClient > rankClient)
      {
       start = (size+1) * rankClient ;
       size++ ;
      }
      else start = size*rankClient + nbServer%nbClient ;
     
      for(int i=0; i<size; i++)
      { 
        int rank=start+i ; 
        size_t indSize = ni_glo/nbServer ;
        size_t indStart ;
        if (ni_glo % nbServer > rank)
        {
          indStart = (indSize+1) * rank ;
          indSize++ ;
        }
        else indStart = indSize*rank + ni_glo%nbServer ;
       
        auto& globalInd =  globalIndex[rank] ;
        globalInd.resize(indSize*nj_glo) ;
        size_t n=0 ;
        for(int j=0; j<nj_glo;j++)
        {
          for(int i=0; i<indSize; i++, n++)  globalInd(n)=indStart+i ;
          indStart=indStart+ni_glo ;  
        }
      }
    }
    else if (distType==EDistributionType::NONE) // domain is not distributed ie all servers get the same local domain
    {
      int nbServer = client->serverSize;
      int nglo=ni_glo*nj_glo ;
      CArray<size_t,1> indGlo ;
      for(size_t i=0;i<nglo;i++) indGlo(i) = i ;
      for (auto& rankServer : client->getRanksServerLeader()) globalIndex[rankServer] = indGlo ; 
    }
    remoteElement_[client] = make_shared<CDistributedElement>(ni_glo*nj_glo, globalIndex) ;
    remoteElement_[client]->addFullView() ;
  }
  CATCH

 

  void CDomain::distributeToServer(CContextClient* client, map<int, CArray<size_t,1>>& globalIndexOut, std::map<int, CArray<size_t,1>>& globalIndexIn,
                                   shared_ptr<CScattererConnector> &scattererConnector, const string& domainId)
  TRY
  {
    string serverDomainId = domainId.empty() ? this->getId() : domainId ;
    CContext* context = CContext::getCurrent();

    this->sendAllAttributesToServer(client, serverDomainId)  ;

    auto scatteredElement = make_shared<CDistributedElement>(ni_glo*nj_glo, globalIndexOut) ;
    scatteredElement->addFullView() ;
    scattererConnector = make_shared<CScattererConnector>(localElement_->getView(CElementView::FULL), scatteredElement->getView(CElementView::FULL), 
                                                          context->getIntraComm(), client->getRemoteSize()) ;
    scattererConnector->computeConnector() ;

    // phase 0
    // send remote element to construct the full view on server, ie without hole 
    CEventClient event0(getType(), EVENT_ID_DOMAIN_DISTRIBUTION);
    CMessage message0 ;
    message0<<serverDomainId<<0 ; 
    remoteElement_[client]->sendToServer(client,event0,message0) ; 
    
    // phase 1
    // send the full view of element to construct the connector which connect distributed data coming from client to the full local view
    CEventClient event1(getType(), EVENT_ID_DOMAIN_DISTRIBUTION);
    CMessage message1 ;
    message1<<serverDomainId<<1<<localElement_->getView(CElementView::FULL)->getGlobalSize() ; 
    scattererConnector->transfer(localElement_->getView(CElementView::FULL)->getGlobalIndex(),client,event1,message1) ;
    
    sendDistributedAttributes(client, scattererConnector, domainId) ;

  
    // phase 2 send the mask : data index + mask2D
    {
      CArray<bool,1> maskIn(localElement_->getView(CElementView::WORKFLOW)->getSize());
      CArray<bool,1> maskOut ;
      auto workflowToFull = make_shared<CLocalConnector>(localElement_->getView(CElementView::WORKFLOW), localElement_->getView(CElementView::FULL)) ;
      workflowToFull->computeConnector() ;
      maskIn=true ;
      workflowToFull->transfer(maskIn,maskOut,false) ;


      // prepare grid scatterer connector to send data from client to server
      map<int,CArray<size_t,1>> workflowGlobalIndex ;
      map<int,CArray<bool,1>> maskOut2 ; 
      scattererConnector->transfer(maskOut, maskOut2, false) ;
      scatteredElement->addView(CElementView::WORKFLOW, maskOut2) ;
      scatteredElement->getView(CElementView::WORKFLOW)->getGlobalIndexView(workflowGlobalIndex) ;
      // create new workflow view for scattered element
      auto clientToServerElement = make_shared<CDistributedElement>(scatteredElement->getGlobalSize(), workflowGlobalIndex) ;
      clientToServerElement->addFullView() ;
      CEventClient event2(getType(), EVENT_ID_DOMAIN_DISTRIBUTION);
      CMessage message2 ;
      message2<<serverDomainId<<2 ; 
      clientToServerElement->sendToServer(client, event2, message2) ; 
      clientToServerConnector_[client] = make_shared<CScattererConnector>(localElement_->getView(CElementView::WORKFLOW), clientToServerElement->getView(CElementView::FULL),
                                                                        context->getIntraComm(), client->getRemoteSize()) ;
    clientToServerConnector_[client]->computeConnector() ;
    }
    ////////////
    // phase 3 : compute connector to receive from server
    ////////////
    {
      auto scatteredElement = make_shared<CDistributedElement>(ni_glo*nj_glo, globalIndexIn) ;
      scatteredElement->addFullView() ;
      auto scattererConnector = make_shared<CScattererConnector>(localElement_->getView(CElementView::FULL), scatteredElement->getView(CElementView::FULL), 
                                                                 context->getIntraComm(), client->getRemoteSize()) ;
      scattererConnector->computeConnector() ;

      CArray<bool,1> maskIn(localElement_->getView(CElementView::WORKFLOW)->getSize());
      CArray<bool,1> maskOut ;
      auto workflowToFull = make_shared<CLocalConnector>(localElement_->getView(CElementView::WORKFLOW), localElement_->getView(CElementView::FULL)) ;
      workflowToFull->computeConnector() ;
      maskIn=true ;
      workflowToFull->transfer(maskIn,maskOut,false) ;

      map<int,CArray<size_t,1>> workflowGlobalIndex ;
      map<int,CArray<bool,1>> maskOut2 ; 
      scattererConnector->transfer(maskOut, maskOut2, false) ;
      scatteredElement->addView(CElementView::WORKFLOW, maskOut2) ;
      scatteredElement->getView(CElementView::WORKFLOW)->getGlobalIndexView(workflowGlobalIndex) ;
      auto clientToServerElement = make_shared<CDistributedElement>(scatteredElement->getGlobalSize(), workflowGlobalIndex) ;
      clientToServerElement->addFullView() ;
      CEventClient event3(getType(), EVENT_ID_DOMAIN_DISTRIBUTION);
      CMessage message3 ;
      message3<<serverDomainId<<3 ; 
      clientToServerElement->sendToServer(client, event3, message3) ; 

      clientFromServerConnector_[client] = make_shared<CGathererConnector>(clientToServerElement->getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW));
      clientFromServerConnector_[client]->computeConnector() ;      
    }
  }
  CATCH
 
  void CDomain::recvDomainDistribution(CEventServer& event)
  TRY
  {
    string domainId;
    int phasis ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> domainId >> phasis ;
    get(domainId)->receivedDomainDistribution(event, phasis);
  }
  CATCH

  void CDomain::receivedDomainDistribution(CEventServer& event, int phasis)
  TRY
  {
    CContext* context = CContext::getCurrent();
    if (phasis==0) // receive the remote element to construct the full view
    {
      localElement_ = make_shared<CLocalElement>(context->getIntraCommRank(),event) ;
      localElement_->addFullView() ;
      // construct the local dimension and indexes
      auto& globalIndex=localElement_->getGlobalIndex() ;
      int nij=globalIndex.numElements() ;
      int minI=ni_glo,maxI=-1,minJ=nj_glo,maxJ=-1 ;
      int i,j ;
      int niGlo=ni_glo, njGlo=njGlo ;
      for(int ij=0;ij<nij;ij++)
      {
        j=globalIndex(ij)/niGlo ;
        i=globalIndex(ij)%niGlo ;
        if (i<minI) minI=i ;
        if (i>maxI) maxI=i ;
        if (j<minJ) minJ=j ;
        if (j>maxJ) maxJ=j ;
      }  
      if (maxI>=minI) { ibegin=minI ; ni=maxI-minI+1 ; }
      else {ibegin=0; ni=0 ;}
      if (maxJ>=minJ) { jbegin=minJ ; nj=maxJ-minJ+1 ; }
      else {jbegin=0; nj=0 ;}

    }
    else if (phasis==1) // receive the sent view from client to construct the full distributed full view on server
    {
      CContext* context = CContext::getCurrent();
      shared_ptr<CDistributedElement> elementFrom = make_shared<CDistributedElement>(event) ;
      elementFrom->addFullView() ;
      gathererConnector_ = make_shared<CGathererConnector>(elementFrom->getView(CElementView::FULL), localElement_->getView(CElementView::FULL)) ;
      gathererConnector_->computeConnector() ; 
    }
    else if (phasis==2)
    {
      elementFrom_ = make_shared<CDistributedElement>(event) ;
      elementFrom_->addFullView() ;
    }
    else if (phasis==3)
    {
      elementTo_ = make_shared<CDistributedElement>(event) ;
      elementTo_->addFullView() ;
    }
  }
  CATCH

  void CDomain::setServerMask(CArray<bool,1>& serverMask, CContextClient* client)
  TRY
  {
    // nota : the client is needed to get the remote size for the scatterer connector. Maybe it is not the good place for this
    // Later, server to client connector can be computed on demand, with "client" as argument
    CContext* context = CContext::getCurrent();
    localElement_->addView(CElementView::WORKFLOW, serverMask) ;
    mask_1d.reference(serverMask.copy()) ;
 
    serverFromClientConnector_ = make_shared<CGathererConnector>(elementFrom_->getView(CElementView::FULL), localElement_->getView(CElementView::WORKFLOW)) ;
    serverFromClientConnector_->computeConnector() ;
      
    serverToClientConnector_ = make_shared<CScattererConnector>(localElement_->getView(CElementView::WORKFLOW), elementTo_->getView(CElementView::FULL),
                                                                context->getIntraComm(), client->getRemoteSize()) ;
    serverToClientConnector_->computeConnector() ;
  }
  CATCH_DUMP_ATTR


  void CDomain::sendDistributedAttributes(CContextClient* client, shared_ptr<CScattererConnector> scattererConnector,  const string& domainId)
  {
    string serverDomainId = domainId.empty() ? this->getId() : domainId ;
    CContext* context = CContext::getCurrent();

    if (hasLonLat)
    {
      { // send longitude
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverDomainId<<string("lon") ; 
        scattererConnector->transfer(lonvalue, client, event,message) ;
      }
      
      { // send latitude
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverDomainId<<string("lat") ; 
        scattererConnector->transfer(latvalue, client, event, message) ;
      }
    }

    if (hasBounds)
    { 
      { // send longitude boudaries
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverDomainId<<string("boundslon") ; 
        scattererConnector->transfer(nvertex, bounds_lonvalue, client, event, message ) ;
      }

      { // send latitude boudaries
        CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
        CMessage message ;
        message<<serverDomainId<<string("boundslat") ; 
        scattererConnector->transfer(nvertex, bounds_latvalue, client, event, message ) ;
      }
    }

    if (hasArea)
    {  // send area
      CEventClient event(getType(), EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE);
      CMessage message ;
      message<<serverDomainId<<string("area") ; 
      scattererConnector->transfer(areavalue, client, event,message) ;
    }
  }

  void CDomain::recvDistributedAttributes(CEventServer& event)
  TRY
  {
    string domainId;
    string type ;
    for (auto& subEvent : event.subEvents) (*subEvent.buffer) >> domainId >> type ;
    get(domainId)->recvDistributedAttributes(event, type);
  }
  CATCH

  void CDomain::recvDistributedAttributes(CEventServer& event, const string& type)
  TRY
  {
    if (type=="lon") 
    {
      CArray<double,1> value ;
      gathererConnector_->transfer(event, value, 0.); 
      lonvalue_2d.resize(ni,nj) ;
      if (lonvalue_2d.numElements()>0) lonvalue_2d=CArray<double,2>(value.dataFirst(),shape(ni,nj),neverDeleteData) ; 
    }
    else if (type=="lat")
    {
      CArray<double,1> value ;
      gathererConnector_->transfer(event, value, 0.); 
      latvalue_2d.resize(ni,nj) ;
      if (latvalue_2d.numElements()>0) latvalue_2d=CArray<double,2>(value.dataFirst(),shape(ni,nj),neverDeleteData) ; 
    }
    else if (type=="boundslon")
    {
      CArray<double,1> value ;
      gathererConnector_->transfer(event, nvertex, value, 0.); 
      bounds_lon_2d.resize(nvertex,ni,nj) ;
      if (bounds_lon_2d.numElements()>0) bounds_lon_2d=CArray<double,3>(value.dataFirst(),shape(nvertex,ni,nj),neverDeleteData) ; 
    }
    else if (type=="boundslat")
    {
      CArray<double,1> value ;
      gathererConnector_->transfer(event, nvertex, value, 0.); 
      bounds_lat_2d.resize(nvertex,ni,nj) ;
      if (bounds_lat_2d.numElements()>0) bounds_lat_2d=CArray<double,3>(value.dataFirst(),shape(nvertex,ni,nj),neverDeleteData) ; 
    }
    else if (type=="area") 
    {
      CArray<double,1> value ;
      gathererConnector_->transfer(event, value, 0.); 
      area.resize(ni,nj) ;
      if (area.numElements()>0) area=CArray<double,2>(value.dataFirst(),shape(ni,nj),neverDeleteData) ; 
    }
  }
  CATCH
    
  bool CDomain::dispatchEvent(CEventServer& event)
  TRY
  {
    if (SuperClass::dispatchEvent(event)) return true;
    else
    {
      switch(event.type)
      {
        case EVENT_ID_DOMAIN_DISTRIBUTION:
          recvDomainDistribution(event);
          return true;
          break;
        case EVENT_ID_SEND_DISTRIBUTED_ATTRIBUTE:
          recvDistributedAttributes(event);
          return true;
          break;  
        default:
          ERROR("bool CDomain::dispatchEvent(CEventServer& event)",
                << "Unknown Event");
          return false;
       }
    }
  }
  CATCH

  
  /*!
    Compare two domain objects. 
    They are equal if only if they have identical attributes as well as their values.
    Moreover, they must have the same transformations.
  \param [in] domain Compared domain
  \return result of the comparison
  */
  bool CDomain::isEqual(CDomain* obj)
  TRY
  {
    vector<StdString> excludedAttr;
    excludedAttr.push_back("domain_ref");
    bool objEqual = SuperClass::isEqual(obj, excludedAttr);
    if (!objEqual) return objEqual;

    TransMapTypes thisTrans = this->getAllTransformations();
    TransMapTypes objTrans  = obj->getAllTransformations();

    TransMapTypes::const_iterator it, itb, ite;
    std::vector<ETranformationType> thisTransType, objTransType;
    for (it = thisTrans.begin(); it != thisTrans.end(); ++it)
      thisTransType.push_back(it->first);
    for (it = objTrans.begin(); it != objTrans.end(); ++it)
      objTransType.push_back(it->first);

    if (thisTransType.size() != objTransType.size()) return false;
    for (int idx = 0; idx < thisTransType.size(); ++idx)
      objEqual &= (thisTransType[idx] == objTransType[idx]);

    return objEqual;
  }
  CATCH_DUMP_ATTR

/////////////////////////////////////////////////////////////////////////
///////////////             TRANSFORMATIONS                    //////////
/////////////////////////////////////////////////////////////////////////

  std::map<StdString, ETranformationType> CDomain::transformationMapList_ = std::map<StdString, ETranformationType>();
  bool CDomain::dummyTransformationMapList_ = CDomain::initializeTransformationMap(CDomain::transformationMapList_);

  bool CDomain::initializeTransformationMap(std::map<StdString, ETranformationType>& m)
  TRY
  {
    m["zoom_domain"] = TRANS_ZOOM_DOMAIN;
    m["interpolate_domain"] = TRANS_INTERPOLATE_DOMAIN;
    m["generate_rectilinear_domain"] = TRANS_GENERATE_RECTILINEAR_DOMAIN;
    m["compute_connectivity_domain"] = TRANS_COMPUTE_CONNECTIVITY_DOMAIN;
    m["expand_domain"] = TRANS_EXPAND_DOMAIN;
    m["reorder_domain"] = TRANS_REORDER_DOMAIN;
    m["extract_domain"] = TRANS_EXTRACT_DOMAIN;
    return true;
  }
  CATCH


  CTransformation<CDomain>* CDomain::addTransformation(ETranformationType transType, const StdString& id)
  TRY
  {
    transformationMap_.push_back(std::make_pair(transType, CTransformation<CDomain>::createTransformation(transType,id)));
    return transformationMap_.back().second;
  }
  CATCH_DUMP_ATTR

  CTransformation<CDomain>* CDomain::addTransformation(ETranformationType transType, CTransformation<CDomain>* transformation)
  TRY
  {
    transformationMap_.push_back(std::make_pair(transType, transformation));
    return transformationMap_.back().second;
  }
  CATCH_DUMP_ATTR
  /*!
    Check whether a domain has transformation
    \return true if domain has transformation
  */
  bool CDomain::hasTransformation()
  TRY
  {
    return (!transformationMap_.empty());
  }
  CATCH_DUMP_ATTR

  /*!
    Set transformation for current domain. It's the method to move transformation in hierarchy
    \param [in] domTrans transformation on domain
  */
  void CDomain::setTransformations(const TransMapTypes& domTrans)
  TRY
  {
    transformationMap_ = domTrans;
  }
  CATCH_DUMP_ATTR

  /*!
    Get all transformation current domain has
    \return all transformation
  */
  CDomain::TransMapTypes CDomain::getAllTransformations(void)
  TRY
  {
    return transformationMap_;
  }
  CATCH_DUMP_ATTR

  void CDomain::duplicateTransformation(CDomain* src)
  TRY
  {
    if (src->hasTransformation())
    {
      this->setTransformations(src->getAllTransformations());
    }
  }
  CATCH_DUMP_ATTR
    
  /*!
   * Go through the hierarchy to find the domain from which the transformations must be inherited
   */
  void CDomain::solveInheritanceTransformation_old()
  TRY
  {
    if (hasTransformation() || !hasDirectDomainReference())
      return;

    CDomain* domain = this;
    std::vector<CDomain*> refDomains;
    while (!domain->hasTransformation() && domain->hasDirectDomainReference())
    {
      refDomains.push_back(domain);
      domain = domain->getDirectDomainReference();
    }

    if (domain->hasTransformation())
      for (size_t i = 0; i < refDomains.size(); ++i)
        refDomains[i]->setTransformations(domain->getAllTransformations());
  }
  CATCH_DUMP_ATTR


  void CDomain::solveInheritanceTransformation()
  TRY
  {
    if (solveInheritanceTransformation_done_) return;
    else solveInheritanceTransformation_done_=true ;

    CDomain* domain = this;
    CDomain* Lastdomain ;
    std::list<CDomain*> refDomains;
    bool out=false ;
    vector<StdString> excludedAttr;
    excludedAttr.push_back("domain_ref");
    
    refDomains.push_front(domain) ;
    while (domain->hasDirectDomainReference() && !out)
    {
      CDomain* lastDomain=domain ;
      domain = domain->getDirectDomainReference();
      domain->solveRefInheritance() ;
      if (!domain->SuperClass::isEqual(lastDomain,excludedAttr)) out=true ;
      refDomains.push_front(domain) ;
    }

    CTransformationPaths::TPath path ;
    auto& pathList = std::get<2>(path) ;
    std::get<0>(path) = EElement::DOMAIN ;
    std::get<1>(path) = refDomains.front()->getId() ;
    for (auto& domain : refDomains)
    {
      CDomain::TransMapTypes transformations = domain->getAllTransformations();
      for(auto& transformation : transformations) pathList.push_back({transformation.second->getTransformationType(), 
                                                                      transformation.second->getId()}) ;
    }
    transformationPaths_.addPath(path) ;

  }
  CATCH_DUMP_ATTR
  

  bool CDomain::activateFieldWorkflow(CGarbageCollector& gc)
  TRY
  {
    if (!domain_ref.isEmpty())
    {
      CField* field=getFieldFromId(domain_ref) ;
      if (field!=nullptr)
      {
        bool ret = field->buildWorkflowGraph(gc) ;
        if (!ret) return false ; // cannot build workflow graph at this state
      }
      else 
      {
        CDomain* domain = get(domain_ref) ;
        bool ret = domain->activateFieldWorkflow(gc) ;
        if (!ret) return false ; // cannot build workflow graph at this state
        domain_ref=domain->getId() ; // replace domain_ref by solved reference
      }
    }
    activateFieldWorkflow_done_=true ;
    return true ;
  }
  CATCH_DUMP_ATTR
/////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////// 

  void CDomain::setContextClient(CContextClient* contextClient)
  TRY
  {
    if (clientsSet.find(contextClient)==clientsSet.end())
    {
      clients.push_back(contextClient) ;
      clientsSet.insert(contextClient);
    }
  }
  CATCH_DUMP_ATTR

  /*!
    Parse children nodes of a domain in xml file.
    Whenver there is a new transformation, its type and name should be added into this function
    \param node child node to process
  */
  void CDomain::parse(xml::CXMLNode & node)
  TRY
  {
    SuperClass::parse(node);

    if (node.goToChildElement())
    {
      StdString nodeElementName;
      do
      {
        StdString nodeId("");
        if (node.getAttributes().end() != node.getAttributes().find("id"))
        { nodeId = node.getAttributes()["id"]; }

        nodeElementName = node.getElementName();
        std::map<StdString, ETranformationType>::const_iterator ite = transformationMapList_.end(), it;
        it = transformationMapList_.find(nodeElementName);
        if (ite != it)
        {
          transformationMap_.push_back(std::make_pair(it->second, CTransformation<CDomain>::createTransformation(it->second,
                                                                                                                nodeId,
                                                                                                                &node)));
        }
        else
        {
          ERROR("void CDomain::parse(xml::CXMLNode & node)",
                << "The transformation " << nodeElementName << " has not been supported yet.");
        }
      } while (node.goToNextElement()) ;
      node.goToParentElement();
    }
  }
  CATCH_DUMP_ATTR
   //----------------------------------------------------------------

   DEFINE_REF_FUNC(Domain,domain)

   ///---------------------------------------------------------------

} // namespace xios
