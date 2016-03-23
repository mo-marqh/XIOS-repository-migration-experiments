/*!
   \file domain_algorithm_interpolate_from_file.cpp
   \author Ha NGUYEN
   \since 09 Jul 2015
   \date 15 Sep 2015

   \brief Algorithm for interpolation on a domain.
 */
#include "domain_algorithm_interpolate.hpp"
#include <boost/unordered_map.hpp>
#include "context.hpp"
#include "context_client.hpp"
#include "distribution_client.hpp"
#include "client_server_mapping_distributed.hpp"
#include "netcdf.hpp"
#include "mapper.hpp"
#include "mpi_tag.hpp"

namespace xios {

CDomainAlgorithmInterpolate::CDomainAlgorithmInterpolate(CDomain* domainDestination, CDomain* domainSource, CInterpolateDomain* interpDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource), interpDomain_(interpDomain)
{
  interpDomain_->checkValid(domainSource);
//  computeIndexSourceMapping();
}

/*!
  Compute remap with integrated remap calculation module
*/
void CDomainAlgorithmInterpolate::computeRemap()
{
  using namespace sphereRemap;

  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int i, j, k, idx;
  std::vector<double> srcPole(3,0), dstPole(3,0);
	int orderInterp = interpDomain_->order.getValue();

  const double poleValue = 90.0;
  const int constNVertex = 4; // Value by default number of vertex for rectangular domain
  int nVertexSrc, nVertexDest;
  nVertexSrc = nVertexDest = constNVertex;

  // First of all, try to retrieve the boundary values of domain source and domain destination
  int localDomainSrcSize = domainSrc_->i_index.numElements();
  int niSrc = domainSrc_->ni.getValue(), njSrc = domainSrc_->nj.getValue();
  bool hasBoundSrc = domainSrc_->hasBounds;
  if (hasBoundSrc) nVertexSrc = domainSrc_->nvertex.getValue();
  CArray<double,2> boundsLonSrc(nVertexSrc,localDomainSrcSize);
  CArray<double,2> boundsLatSrc(nVertexSrc,localDomainSrcSize);

  if (CDomain::type_attr::rectilinear == domainSrc_->type) srcPole[2] = 1;
  if (hasBoundSrc)  // Suppose that domain source is curvilinear or unstructured
  {
    if (!domainSrc_->bounds_lon_2d.isEmpty())
    {
      for (j = 0; j < njSrc; ++j)
        for (i = 0; i < niSrc; ++i)
        {
          k=j*niSrc+i;
          for(int n=0;n<nVertexSrc;++n)
          {
            boundsLonSrc(n,k) = domainSrc_->bounds_lon_2d(n,i,j);
            boundsLatSrc(n,k) = domainSrc_->bounds_lat_2d(n,i,j);
          }
        }
    }
    else
    {
      boundsLonSrc = domainSrc_->bounds_lon_1d;
      boundsLatSrc = domainSrc_->bounds_lat_1d;
    }
  }
  else // if domain source is rectilinear, not do anything now
  {
    bool isNorthPole = false;
    bool isSouthPole = false;
    CArray<double,1> lon_g ;
    CArray<double,1> lat_g ;

    if (!domainSrc_->lonvalue_1d.isEmpty() && !domainSrc_->latvalue_1d.isEmpty())
    {
		domainSrc_->AllgatherRectilinearLonLat(domainSrc_->lonvalue_1d,domainSrc_->latvalue_1d, lon_g,lat_g) ;
	}
	else if (! domainSrc_->latvalue_rectilinear_read_from_file.isEmpty() && ! domainSrc_->lonvalue_rectilinear_read_from_file.isEmpty() )
    {
	  	lat_g=domainSrc_->latvalue_rectilinear_read_from_file ;
	  	lon_g=domainSrc_->lonvalue_rectilinear_read_from_file ;
	}
	else if (!domainSrc_->lon_start.isEmpty() && !domainSrc_->lon_end.isEmpty() &&
	         !domainSrc_->lat_start.isEmpty() && !domainSrc_->lat_end.isEmpty())
	{
	  double step=(domainSrc_->lon_end-domainSrc_->lon_start)/domainSrc_->ni_glo ;
	  for(int i=0; i<domainSrc_->ni_glo; ++i) lon_g(i)=domainSrc_->lon_start+i*step ;
	  step=(domainSrc_->lat_end-domainSrc_->lat_start)/domainSrc_->nj_glo ;
	  for(int i=0; i<domainSrc_->ni_glo; ++i) lat_g(i)=domainSrc_->lat_start+i*step ;
	}
	else ERROR("void CDomainAlgorithmInterpolate::computeRemap()",<<"Cannot compute bounds for rectilinear domain") ;

    nVertexSrc = constNVertex;
    domainSrc_->fillInRectilinearBoundLonLat(lon_g,lat_g, boundsLonSrc, boundsLatSrc);
  }

  std::map<int,std::vector<std::pair<int,double> > > interpMapValueNorthPole;
  std::map<int,std::vector<std::pair<int,double> > > interpMapValueSouthPole;

  int localDomainDestSize = domainDest_->i_index.numElements();
  int niDest = domainDest_->ni.getValue(), njDest = domainDest_->nj.getValue();
  bool hasBoundDest = domainDest_->hasBounds;
  if (hasBoundDest) nVertexDest = domainDest_->nvertex.getValue();
  CArray<double,2> boundsLonDest(nVertexDest,localDomainDestSize);
  CArray<double,2> boundsLatDest(nVertexDest,localDomainDestSize);

  if (CDomain::type_attr::rectilinear == domainDest_->type) dstPole[2] = 1;
  if (hasBoundDest)
  {
    if (!domainDest_->bounds_lon_2d.isEmpty())
    {
      for (j = 0; j < njDest; ++j)
        for (i = 0; i < niDest; ++i)
        {
          k=j*niDest+i;
          for(int n=0;n<nVertexDest;++n)
          {
            boundsLonDest(n,k) = domainDest_->bounds_lon_2d(n,i,j);
            boundsLatDest(n,k) = domainDest_->bounds_lat_2d(n,i,j);
          }
        }
    }
    else
    {
      boundsLonDest = domainDest_->bounds_lon_1d;
      boundsLatDest = domainDest_->bounds_lat_1d;
    }
  }
  else
  {
    bool isNorthPole = false;
    bool isSouthPole = false;
    if (std::abs(poleValue - std::abs(domainDest_->lat_start)) < NumTraits<double>::epsilon()) isNorthPole = true;
    if (std::abs(poleValue - std::abs(domainDest_->lat_end)) < NumTraits<double>::epsilon()) isSouthPole = true;

    CArray<double,1> lon_g ;
    CArray<double,1> lat_g ;

    if (!domainDest_->lonvalue_1d.isEmpty() && !domainDest_->latvalue_1d.isEmpty())
    {
		domainDest_->AllgatherRectilinearLonLat(domainDest_->lonvalue_1d,domainDest_->latvalue_1d, lon_g,lat_g) ;
	}
	else if (! domainDest_->latvalue_rectilinear_read_from_file.isEmpty() && ! domainDest_->lonvalue_rectilinear_read_from_file.isEmpty() )
    {
	  	lat_g=domainDest_->latvalue_rectilinear_read_from_file ;
	  	lon_g=domainDest_->lonvalue_rectilinear_read_from_file ;
	}
	else if (!domainDest_->lon_start.isEmpty() && !domainDest_->lon_end.isEmpty() &&
	         !domainDest_->lat_start.isEmpty() && !domainDest_->lat_end.isEmpty())
	{
	  double step=(domainDest_->lon_end-domainDest_->lon_start)/domainDest_->ni_glo ;
	  for(int i=0; i<domainDest_->ni_glo; ++i) lon_g(i)=domainDest_->lon_start+i*step ;
	  step=(domainDest_->lat_end-domainDest_->lat_start)/domainDest_->nj_glo ;
	  for(int i=0; i<domainDest_->ni_glo; ++i) lat_g(i)=domainDest_->lat_start+i*step ;
	}
	else ERROR("void CDomainAlgorithmInterpolate::computeRemap()",<<"Cannot compute bounds for rectilinear domain") ;
    if (std::abs(poleValue - std::abs(lat_g(0))) < NumTraits<double>::epsilon()) isNorthPole = true;
    if (std::abs(poleValue - std::abs(lat_g(domainDest_->nj_glo-1))) < NumTraits<double>::epsilon()) isSouthPole = true;




    if (isNorthPole && (0 == domainDest_->jbegin.getValue()))
    {
      int ibegin = domainDest_->ibegin.getValue();
      for (i = 0; i < niDest; ++i)
      {
        interpMapValueNorthPole[i+ibegin];
      }
    }

    if (isSouthPole && (domainDest_->nj_glo.getValue() == (domainDest_->jbegin.getValue() + njDest)))
    {
      int ibegin = domainDest_->ibegin.getValue();
      int njGlo = domainDest_->nj_glo.getValue();
      int niGlo = domainDest_->ni_glo.getValue();
      for (i = 0; i < niDest; ++i)
      {
        k = (njGlo - 1)*niGlo + i + ibegin;
        interpMapValueSouthPole[k];
      }
    }

    // Ok, fill in boundary values for rectangular domain
    nVertexDest = constNVertex;
    domainDest_->fillInRectilinearBoundLonLat(lon_g,lat_g, boundsLonDest, boundsLatDest);
  }



  // Ok, now use mapper to calculate
  int nSrcLocal = domainSrc_->i_index.numElements();
  int nDstLocal = domainDest_->i_index.numElements();
  long int * globalSrc = new long int [nSrcLocal];
  long int * globalDst = new long int [nDstLocal];

  long int globalIndex;
  int i_ind, j_ind;
  for (int idx = 0; idx < nSrcLocal; ++idx)
  {
    i_ind=domainSrc_->i_index(idx) ;
    j_ind=domainSrc_->j_index(idx) ;

    globalIndex = i_ind + j_ind * domainSrc_->ni_glo;
    globalSrc[idx] = globalIndex;
  }

  for (int idx = 0; idx < nDstLocal; ++idx)
  {
    i_ind=domainDest_->i_index(idx) ;
    j_ind=domainDest_->j_index(idx) ;

    globalIndex = i_ind + j_ind * domainDest_->ni_glo;
    globalDst[idx] = globalIndex;
  }


  // Calculate weight index
  Mapper mapper(client->intraComm);
  mapper.setVerbosity(PROGRESS) ;
  mapper.setSourceMesh(boundsLonSrc.dataFirst(), boundsLatSrc.dataFirst(), nVertexSrc, nSrcLocal, &srcPole[0], globalSrc);
  mapper.setTargetMesh(boundsLonDest.dataFirst(), boundsLatDest.dataFirst(), nVertexDest, nDstLocal, &dstPole[0], globalDst);
  std::vector<double> timings = mapper.computeWeights(orderInterp);

  std::map<int,std::vector<std::pair<int,double> > > interpMapValue;
  std::map<int,std::vector<std::pair<int,double> > >::const_iterator iteNorthPole = interpMapValueNorthPole.end(),
                                                                     iteSouthPole = interpMapValueSouthPole.end();
  for (int idx = 0;  idx < mapper.nWeights; ++idx)
  {
    interpMapValue[mapper.targetWeightId[idx]].push_back(make_pair(mapper.sourceWeightId[idx],mapper.remapMatrix[idx]));
    if (iteNorthPole != interpMapValueNorthPole.find(mapper.targetWeightId[idx]))
    {
      interpMapValueNorthPole[mapper.targetWeightId[idx]].push_back(make_pair(mapper.sourceWeightId[idx],mapper.remapMatrix[idx]));
    }

    if (iteSouthPole != interpMapValueSouthPole.find(mapper.targetWeightId[idx]))
    {
      interpMapValueSouthPole[mapper.targetWeightId[idx]].push_back(make_pair(mapper.sourceWeightId[idx],mapper.remapMatrix[idx]));
    }
  }
  int niGloDst = domainDest_->ni_glo.getValue();
  processPole(interpMapValueNorthPole, niGloDst);
  processPole(interpMapValueSouthPole, niGloDst);

  if (!interpMapValueNorthPole.empty())
  {
     std::map<int,std::vector<std::pair<int,double> > >::iterator itNorthPole = interpMapValueNorthPole.begin();
     for (; itNorthPole != iteNorthPole; ++itNorthPole)
     {
       if (!(itNorthPole->second.empty()))
        itNorthPole->second.swap(interpMapValue[itNorthPole->first]);
     }
  }

  if (!interpMapValueSouthPole.empty())
  {
     std::map<int,std::vector<std::pair<int,double> > >::iterator itSouthPole = interpMapValueSouthPole.begin();
     for (; itSouthPole != iteSouthPole; ++itSouthPole)
     {
       if (!(itSouthPole->second.empty()))
        itSouthPole->second.swap(interpMapValue[itSouthPole->first]);
     }
  }

  exchangeRemapInfo(interpMapValue);

  delete [] globalSrc;
  delete [] globalDst;
}

void CDomainAlgorithmInterpolate::processPole(std::map<int,std::vector<std::pair<int,double> > >& interMapValuePole,
                                              int nbGlobalPointOnPole)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  MPI_Comm poleComme(MPI_COMM_NULL);
  MPI_Comm_split(client->intraComm, interMapValuePole.empty() ? MPI_UNDEFINED : 1, 0, &poleComme);
  if (MPI_COMM_NULL != poleComme)
  {
    int nbClientPole;
    MPI_Comm_size(poleComme, &nbClientPole);

    std::map<int,std::vector<std::pair<int,double> > >::iterator itePole = interMapValuePole.end(), itPole,
                                                                 itbPole = interMapValuePole.begin();

    int nbWeight = 0;
    for (itPole = itbPole; itPole != itePole; ++itPole)
       nbWeight += itPole->second.size();

    std::vector<int> recvCount(nbClientPole,0);
    std::vector<int> displ(nbClientPole,0);
    MPI_Allgather(&nbWeight,1,MPI_INT,&recvCount[0],1,MPI_INT,poleComme) ;

    displ[0]=0;
    for(int n=1;n<nbClientPole;++n) displ[n]=displ[n-1]+recvCount[n-1] ;
    int recvSize=displ[nbClientPole-1]+recvCount[nbClientPole-1] ;

    std::vector<int> sendSourceIndexBuff(nbWeight);
    std::vector<double> sendSourceWeightBuff(nbWeight);
    int k = 0;
    for (itPole = itbPole; itPole != itePole; ++itPole)
    {
      for (int idx = 0; idx < itPole->second.size(); ++idx)
      {
        sendSourceIndexBuff[k] = (itPole->second)[idx].first;
        sendSourceWeightBuff[k] = (itPole->second)[idx].second;
        ++k;
      }
    }

    std::vector<int> recvSourceIndexBuff(recvSize);
    std::vector<double> recvSourceWeightBuff(recvSize);

    // Gather all index and weight for pole
    MPI_Allgatherv(&sendSourceIndexBuff[0],nbWeight,MPI_INT,&recvSourceIndexBuff[0],&recvCount[0],&displ[0],MPI_INT,poleComme);
    MPI_Allgatherv(&sendSourceWeightBuff[0],nbWeight,MPI_DOUBLE,&recvSourceWeightBuff[0],&recvCount[0],&displ[0],MPI_DOUBLE,poleComme);

    std::map<int,double> recvTemp;
    for (int idx = 0; idx < recvSize; ++idx)
    {
      if (recvTemp.end() != recvTemp.find(recvSourceIndexBuff[idx]))
        recvTemp[recvSourceIndexBuff[idx]] += recvSourceWeightBuff[idx]/nbGlobalPointOnPole;
      else
        recvTemp[recvSourceIndexBuff[idx]] = 0.0;
    }

    std::map<int,double>::const_iterator itRecvTemp, itbRecvTemp = recvTemp.begin(), iteRecvTemp = recvTemp.end();

    for (itPole = itbPole; itPole != itePole; ++itPole)
    {
      itPole->second.clear();
      for (itRecvTemp = itbRecvTemp; itRecvTemp != iteRecvTemp; ++itRecvTemp)
          itPole->second.push_back(make_pair(itRecvTemp->first, itRecvTemp->second));
    }
  }

}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmInterpolate::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  if (!interpDomain_->file.isEmpty())
    readRemapInfo();
  else
    computeRemap();
}

void CDomainAlgorithmInterpolate::readRemapInfo()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  std::string filename = interpDomain_->file.getValue();
  std::map<int,std::vector<std::pair<int,double> > > interpMapValue;
  readInterpolationInfo(filename, interpMapValue);

  exchangeRemapInfo(interpMapValue);
}


/*!
  Read remap information from file then distribute it among clients
*/
void CDomainAlgorithmInterpolate::exchangeRemapInfo(const std::map<int,std::vector<std::pair<int,double> > >& interpMapValue)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  this->transformationMapping_.resize(1);
  this->transformationWeight_.resize(1);

  std::map<int, std::vector<int> >& transMap = this->transformationMapping_[0];
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_[0];

  boost::unordered_map<size_t,int> globalIndexOfDomainDest;
  int ni = domainDest_->ni.getValue();
  int nj = domainDest_->nj.getValue();
  int ni_glo = domainDest_->ni_glo.getValue();
  size_t globalIndex;
  int nIndexSize = domainDest_->i_index.numElements(), i_ind, j_ind;
  for (int idx = 0; idx < nIndexSize; ++idx)
  {
    i_ind=domainDest_->i_index(idx) ;
    j_ind=domainDest_->j_index(idx) ;

    globalIndex = i_ind + j_ind * ni_glo;
    globalIndexOfDomainDest[globalIndex] = clientRank;
  }

  CClientServerMappingDistributed domainIndexClientClientMapping(globalIndexOfDomainDest,
                                                                 client->intraComm,
                                                                 true);
  CArray<size_t,1> globalIndexInterp(interpMapValue.size());
  std::map<int,std::vector<std::pair<int,double> > >::const_iterator itb = interpMapValue.begin(), it,
                                                                     ite = interpMapValue.end();
  size_t globalIndexCount = 0;
  for (it = itb; it != ite; ++it)
  {
    globalIndexInterp(globalIndexCount) = it->first;
    ++globalIndexCount;
  }

  domainIndexClientClientMapping.computeServerIndexMapping(globalIndexInterp);
  const CClientServerMapping::GlobalIndexMap& globalIndexInterpSendToClient = domainIndexClientClientMapping.getGlobalIndexOnServer();

  //Inform each client number of index they will receive
  int nbClient = client->clientSize;
  int* sendBuff = new int[nbClient];
  int* recvBuff = new int[nbClient];
  for (int i = 0; i < nbClient; ++i)
  {
    sendBuff[i] = 0;
    recvBuff[i] = 0;
  }
  int sendBuffSize = 0;
  CClientServerMapping::GlobalIndexMap::const_iterator itbMap = globalIndexInterpSendToClient.begin(), itMap,
                                                       iteMap = globalIndexInterpSendToClient.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    const std::vector<size_t>& tmp = itMap->second;
    int sizeIndex = 0, mapSize = (itMap->second).size();
    for (int idx = 0; idx < mapSize; ++idx)
    {
      sizeIndex += interpMapValue.at((itMap->second)[idx]).size();
    }
    sendBuff[itMap->first] = sizeIndex;
    sendBuffSize += sizeIndex;
  }


  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_SUM, client->intraComm);

  int* sendIndexDestBuff = new int [sendBuffSize];
  int* sendIndexSrcBuff  = new int [sendBuffSize];
  double* sendWeightBuff = new double [sendBuffSize];

  std::vector<MPI_Request> sendRequest;

  int sendOffSet = 0, l = 0;
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    const std::vector<size_t>& indexToSend = itMap->second;
    int mapSize = indexToSend.size();
    int k = 0;
    for (int idx = 0; idx < mapSize; ++idx)
    {
      const std::vector<std::pair<int,double> >& interpMap = interpMapValue.at(indexToSend[idx]);
      for (int i = 0; i < interpMap.size(); ++i)
      {
        sendIndexDestBuff[l] = indexToSend[idx];
        sendIndexSrcBuff[l]  = interpMap[i].first;
        sendWeightBuff[l]    = interpMap[i].second;
        ++k;
        ++l;
      }
    }

    sendRequest.push_back(MPI_Request());
    MPI_Isend(sendIndexDestBuff + sendOffSet,
             k,
             MPI_INT,
             itMap->first,
             MPI_DOMAIN_INTERPOLATION_DEST_INDEX,
             client->intraComm,
             &sendRequest.back());
    sendRequest.push_back(MPI_Request());
    MPI_Isend(sendIndexSrcBuff + sendOffSet,
             k,
             MPI_INT,
             itMap->first,
             MPI_DOMAIN_INTERPOLATION_SRC_INDEX,
             client->intraComm,
             &sendRequest.back());
    sendRequest.push_back(MPI_Request());
    MPI_Isend(sendWeightBuff + sendOffSet,
             k,
             MPI_DOUBLE,
             itMap->first,
             MPI_DOMAIN_INTERPOLATION_WEIGHT,
             client->intraComm,
             &sendRequest.back());
    sendOffSet += k;
  }

  int recvBuffSize = recvBuff[clientRank];
  int* recvIndexDestBuff = new int [recvBuffSize];
  int* recvIndexSrcBuff  = new int [recvBuffSize];
  double* recvWeightBuff = new double [recvBuffSize];
  int receivedSize = 0;
  int clientSrcRank;
  while (receivedSize < recvBuffSize)
  {
    MPI_Status recvStatus;
    MPI_Recv((recvIndexDestBuff + receivedSize),
             recvBuffSize,
             MPI_INT,
             MPI_ANY_SOURCE,
             MPI_DOMAIN_INTERPOLATION_DEST_INDEX,
             client->intraComm,
             &recvStatus);

    int countBuff = 0;
    MPI_Get_count(&recvStatus, MPI_INT, &countBuff);
    clientSrcRank = recvStatus.MPI_SOURCE;

    MPI_Recv((recvIndexSrcBuff + receivedSize),
             recvBuffSize,
             MPI_INT,
             clientSrcRank,
             MPI_DOMAIN_INTERPOLATION_SRC_INDEX,
             client->intraComm,
             &recvStatus);

    MPI_Recv((recvWeightBuff + receivedSize),
             recvBuffSize,
             MPI_DOUBLE,
             clientSrcRank,
             MPI_DOMAIN_INTERPOLATION_WEIGHT,
             client->intraComm,
             &recvStatus);

    for (int idx = 0; idx < countBuff; ++idx)
    {
      transMap[*(recvIndexDestBuff + receivedSize + idx)].push_back(*(recvIndexSrcBuff + receivedSize + idx));
      transWeight[*(recvIndexDestBuff + receivedSize + idx)].push_back(*(recvWeightBuff + receivedSize + idx));
    }
    receivedSize += countBuff;
  }

  std::vector<MPI_Status> requestStatus(sendRequest.size());
  MPI_Waitall(sendRequest.size(), &sendRequest[0], MPI_STATUS_IGNORE);

  delete [] sendIndexDestBuff;
  delete [] sendIndexSrcBuff;
  delete [] sendWeightBuff;
  delete [] recvIndexDestBuff;
  delete [] recvIndexSrcBuff;
  delete [] recvWeightBuff;
  delete [] sendBuff;
  delete [] recvBuff;
}

/*!
  Read interpolation information from a file
  \param [in] filename interpolation file
  \param [in/out] interpMapValue Mapping between (global) index of domain on grid destination and
         corresponding global index of domain and associated weight value on grid source
*/
void CDomainAlgorithmInterpolate::readInterpolationInfo(std::string& filename,
                                                        std::map<int,std::vector<std::pair<int,double> > >& interpMapValue)
{
  int ncid ;
  int weightDimId ;
  size_t nbWeightGlo ;

  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;
  int clientSize = client->clientSize;

  nc_open(filename.c_str(),NC_NOWRITE, &ncid) ;
  nc_inq_dimid(ncid,"n_weight",&weightDimId) ;
  nc_inq_dimlen(ncid,weightDimId,&nbWeightGlo) ;

  size_t nbWeight ;
  size_t start ;
  size_t div = nbWeightGlo/clientSize ;
  size_t mod = nbWeightGlo%clientSize ;
  if (clientRank < mod)
  {
    nbWeight=div+1 ;
    start=clientRank*(div+1) ;
  }
  else
  {
    nbWeight=div ;
    start= mod * (div+1) + (clientRank-mod) * div ;
  }

  double* weight=new double[nbWeight] ;
  int weightId ;
  nc_inq_varid (ncid, "weight", &weightId) ;
  nc_get_vara_double(ncid, weightId, &start, &nbWeight, weight) ;

  long* srcIndex=new long[nbWeight] ;
  int srcIndexId ;
  nc_inq_varid (ncid, "src_idx", &srcIndexId) ;
  nc_get_vara_long(ncid, srcIndexId, &start, &nbWeight, srcIndex) ;

  long* dstIndex=new long[nbWeight] ;
  int dstIndexId ;
  nc_inq_varid (ncid, "dst_idx", &dstIndexId) ;
  nc_get_vara_long(ncid, dstIndexId, &start, &nbWeight, dstIndex) ;

  for(size_t ind=0; ind<nbWeight;++ind)
    interpMapValue[dstIndex[ind]-1].push_back(make_pair(srcIndex[ind]-1,weight[ind]));
}

}
