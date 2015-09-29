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

namespace xios {

CDomainAlgorithmInterpolate::CDomainAlgorithmInterpolate(CDomain* domainDestination, CDomain* domainSource, CInterpolateDomain* interpDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource), interpDomain_(interpDomain)
{
  interpDomain_->checkValid(domainSource);
  computeIndexSourceMapping();
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

  int constNVertex = 4; // Value by default number of vertex for rectangular domain
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
    nVertexSrc = constNVertex;
    domainSrc_->fillInRectilinearBoundLonLat(boundsLonSrc, boundsLatSrc);
  }

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
    // Ok, fill in boundary values for rectangular domain
    domainDest_->fillInRectilinearBoundLonLat(boundsLonDest, boundsLatDest);
    nVertexDest = constNVertex;
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
  for (int idx = 0;  idx < mapper.nWeights; ++idx)
  {
    interpMapValue[mapper.targetWeightId[idx]].push_back(make_pair(mapper.sourceWeightId[idx],mapper.remapMatrix[idx]));
  }
  exchangeRemapInfo(interpMapValue);

  delete [] globalSrc;
  delete [] globalDst;
}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmInterpolate::computeIndexSourceMapping()
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
  const std::map<int, std::vector<size_t> >& globalIndexInterpSendToClient = domainIndexClientClientMapping.getGlobalIndexOnServer();

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
  std::map<int, std::vector<size_t> >::const_iterator itbMap = globalIndexInterpSendToClient.begin(), itMap,
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
             7,
             client->intraComm,
             &sendRequest.back());
    sendRequest.push_back(MPI_Request());
    MPI_Isend(sendIndexSrcBuff + sendOffSet,
             k,
             MPI_INT,
             itMap->first,
             8,
             client->intraComm,
             &sendRequest.back());
    sendRequest.push_back(MPI_Request());
    MPI_Isend(sendWeightBuff + sendOffSet,
             k,
             MPI_DOUBLE,
             itMap->first,
             9,
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
             7,
             client->intraComm,
             &recvStatus);

    int countBuff = 0;
    MPI_Get_count(&recvStatus, MPI_INT, &countBuff);
    clientSrcRank = recvStatus.MPI_SOURCE;

    MPI_Recv((recvIndexSrcBuff + receivedSize),
             recvBuffSize,
             MPI_INT,
             clientSrcRank,
             8,
             client->intraComm,
             &recvStatus);

    MPI_Recv((recvWeightBuff + receivedSize),
             recvBuffSize,
             MPI_DOUBLE,
             clientSrcRank,
             9,
             client->intraComm,
             &recvStatus);

    for (int idx = 0; idx < countBuff; ++idx)
    {
      transformationMapping_[*(recvIndexDestBuff + receivedSize + idx)].push_back(*(recvIndexSrcBuff + receivedSize + idx));
      transformationWeight_[*(recvIndexDestBuff + receivedSize + idx)].push_back(*(recvWeightBuff + receivedSize + idx));
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
