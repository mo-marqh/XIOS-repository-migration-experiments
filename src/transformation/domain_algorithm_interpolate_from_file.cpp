/*!
   \file domain_algorithm_interpolate_from_file.cpp
   \author Ha NGUYEN
   \since 09 Jul 2015
   \date 17 Jul 2015

   \brief Algorithm for interpolation on a domain.
 */
#include "domain_algorithm_interpolate_from_file.hpp"
#include <boost/unordered_map.hpp>
#include "context.hpp"
#include "context_client.hpp"
#include "distribution_client.hpp"
#include "client_server_mapping_distributed.hpp"
#include "netcdf.hpp"

namespace xios {

CDomainAlgorithmInterpolateFromFile::CDomainAlgorithmInterpolateFromFile(CDomain* domainDestination, CDomain* domainSource, CInterpolateFromFileDomain* interpDomain)
: CDomainAlgorithmTransformation(domainDestination, domainSource), interpDomain_(interpDomain)
{
  interpDomain_->checkValid(domainSource);
  computeIndexSourceMapping();
}

/*!
  Compute the index mapping between domain on grid source and one on grid destination
*/
void CDomainAlgorithmInterpolateFromFile::computeIndexSourceMapping()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int clientRank = client->clientRank;

  std::string filename = interpDomain_->file.getValue();
  std::map<int,std::vector<std::pair<int,double> > > interpMapValue;
  readInterpolationInfo(filename, interpMapValue);

  //randomizeInterpolationInfo(interpMapValue);
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
  for (int i = 0; i < nbClient; ++i) sendBuff[i] = 0;
  int sendBuffSize = 0;
  std::map<int, std::vector<size_t> >::const_iterator itbMap = globalIndexInterpSendToClient.begin(), itMap,
                                                      iteMap = globalIndexInterpSendToClient.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    int sizeIndex = 0, mapSize = (itMap->second).size();
    for (int idx = 0; idx < mapSize; ++idx)
    {
      sizeIndex += interpMapValue[(itMap->second)[idx]].size();
    }
    sendBuff[itMap->first] = sizeIndex;
    sendBuffSize += sizeIndex;
  }

  MPI_Allreduce(sendBuff, recvBuff, nbClient, MPI_INT, MPI_SUM, client->intraComm);

  int* sendIndexDestBuff = new int [sendBuffSize];
  int* sendIndexSrcBuff  = new int [sendBuffSize];
  double* sendWeightBuff = new double [sendBuffSize];

  std::vector<MPI_Request> sendRequest;
  // Now send index and weight
  int sendOffSet = 0;
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    int k = 0;
    int mapSize = (itMap->second).size();

    for (int idx = 0; idx < mapSize; ++idx)
    {
      std::vector<std::pair<int,double> >& interpMap = interpMapValue[(itMap->second)[idx]];
      for (int i = 0; i < interpMap.size(); ++i)
      {
        sendIndexDestBuff[k] = (itMap->second)[idx];
        sendIndexSrcBuff[k]  = interpMap[i].first;
        sendWeightBuff[k]    = interpMap[i].second;
        ++k;
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
  MPI_Wait(&sendRequest[0], &requestStatus[0]);

  delete [] sendIndexDestBuff;
  delete [] sendIndexSrcBuff;
  delete [] sendWeightBuff;
  delete [] recvIndexDestBuff;
  delete [] recvIndexSrcBuff;
  delete [] recvWeightBuff;
  delete [] sendBuff;
  delete [] recvBuff;
}

void CDomainAlgorithmInterpolateFromFile::randomizeInterpolationInfo(std::map<int,std::vector<std::pair<int,double> > >& interpMapValue)
{
  int iDestBegin = 2;
  int jDestBegin = 2;

  int ni_glo_src = domainSrc_->ni_glo.getValue();
  int ni_glo = domainDest_->ni_glo.getValue();
  size_t globalIndex;
  int nIndexSize = domainDest_->i_index.numElements(), i_ind, j_ind;
  for (int idx = 0; idx < nIndexSize; ++idx)
  {
    i_ind=domainDest_->i_index(idx) ;
    j_ind=domainDest_->j_index(idx) ;

    globalIndex = i_ind + j_ind * ni_glo;

    interpMapValue[globalIndex].push_back(make_pair((i_ind+iDestBegin) + (j_ind+jDestBegin-1)*ni_glo_src, 0.25));
    interpMapValue[globalIndex].push_back(make_pair((i_ind+iDestBegin+1) + (j_ind+jDestBegin)*ni_glo_src, 0.25));
    interpMapValue[globalIndex].push_back(make_pair((i_ind+iDestBegin) + (j_ind+jDestBegin+1)*ni_glo_src, 0.25));
    interpMapValue[globalIndex].push_back(make_pair((i_ind+iDestBegin-1) + (j_ind+jDestBegin)*ni_glo_src, 0.25));


  }
}

/*!
  Read interpolation information from a file
  \param [in] filename interpolation file
  \param [in/out] interpMapValue Mapping between (global) index of domain on grid destination and
         corresponding global index of domain and associated weight value on grid source
*/
void CDomainAlgorithmInterpolateFromFile::readInterpolationInfo(std::string& filename,
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
