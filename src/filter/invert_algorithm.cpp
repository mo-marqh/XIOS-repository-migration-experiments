#include "invert_algorithm.hpp"
#include "axis.hpp"
#include "grid.hpp"
#include <vector>
#include <map>
#include <list>
#include <algorithm>
#include "mpi.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "field.hpp"
#include "array_new.hpp"

namespace xios {
CInvertAlgorithm::CInvertAlgorithm()
{
}

void CInvertAlgorithm::operate(CAxisTransformation& axisTransformation)
{
  std::vector<CAxis*> axisInputs = axisTransformation.getInputs();
  std::vector<CAxis*> axisOutputs = axisTransformation.getOutputs();

  // For now, only consider simple case one input - one output
  CAxis* axisInput = axisInputs[0], *axisOutput = axisOutputs[0];
  // Check correct size
  if (axisInput->size.getValue() != axisOutput->size.getValue())
  {
    ERROR("CInvertAlgorithm::operate(CAxisTransformation& axisTransformation)",
           << "Two axis have different size"
           << "Size of axis source " <<axisInput->getId() << " is " << axisInput->size.getValue()  << std::endl
           << "Size of axis destionation " <<axisOutput->getId() << " is " << axisOutput->size.getValue());
  }

  if ((axisInput->ni.isEmpty()) || (axisInput->ni.getValue() == axisInput->size.getValue()))
  {
    // axis non-distributed
    size_t ssize = axisInput->size.getValue();
    axisOutput->value.resize(ssize);
    for (size_t idx = 0; idx < ssize; ++idx)
    {
      (axisOutput->value)(idx) = (axisInput->value)(ssize-idx-1);
    }
  }
  else
  {
    CContext* context = CContext::getCurrent();
    CContextClient* client = context->client;
    int nbClient = client->clientSize;

    // Ok, now axis is distributed
    // First of all, each of client needs to have the whole axis data
    int localAxisSize = axisInput->ni.getValue();
    int* recvAxisSize = new int[nbClient];
    MPI_Allgather(&localAxisSize, 1, MPI_INT,
                  recvAxisSize, 1, MPI_INT, client->intraComm);


    int* displ=new int[nbClient];
    displ[0]=0 ;
    for(int n=1;n<nbClient;n++) displ[n]=displ[n-1]+recvAxisSize[n-1];
    int recvSize=displ[nbClient-1]+recvAxisSize[nbClient-1];
    double* recvBuff=new double[recvSize];

    double* sendBuff = new double[localAxisSize];
    for (int idx = 0; idx < localAxisSize; ++idx) sendBuff[idx] = (axisInput->value)(idx);

    MPI_Allgatherv(sendBuff, localAxisSize, MPI_DOUBLE,
                   recvBuff, recvAxisSize, displ, MPI_DOUBLE,
                   client->intraComm);

    int ibegin = axisInput->ibegin.getValue();
    size_t ssize = axisInput->size.getValue();
    axisOutput->value.resize(localAxisSize);
    for (int idx = 0; idx < localAxisSize; ++idx)
        (axisOutput->value)(idx) = recvBuff[ssize-ibegin-idx-1];

    std::cout << "axisOutput value " << (axisOutput->value) << std::endl;

    delete [] displ;
    delete [] sendBuff;
    delete [] recvBuff;
    delete [] recvAxisSize;
  }
}


void CInvertAlgorithm::operate(CAxisFilter& axisFilter)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;
  MPI_Comm clientIntraComm = client->intraComm;

  CGrid* gridInput = axisFilter.gridInput;
  CGrid* gridOutput = axisFilter.gridOutput;


  bool isDataDistributed = (gridInput->doGridHaveDataDistributed());
  isDataDistributed = false;

  // Data distributed
  const CArray<size_t,1>& globalDataIndexInput = axisFilter.getGlobalDataIndexInput();
  if (sendingIndexMap.empty() && receivingIndexMap.empty() && indexMap.empty())
    inverseGlobalDataIndex(globalDataIndexInput);


  const std::vector<CField*> fieldInputs = axisFilter.getInputs();
  std::vector<CField*> fieldOutputs = axisFilter.getOutputs();

  CField* input  = fieldInputs[0];
  CField* output = fieldOutputs[0];

  CArray<double, 1>& dataInput  = input->data;
  CArray<double, 1>& dataOutput = output->filteredData;

  if (dataOutput.numElements() != dataInput.numElements())
    dataOutput.resize(dataInput.numElements());

  std::map<size_t,size_t>::iterator itb = indexMap.begin(), ite = indexMap.end(), it;
  for (it = itb; it != ite; ++it)
  {
    dataOutput(it->first) = dataInput(it->second);
  }

   std::cout << "dataOutput 1 "<<  dataOutput <<  std::endl;
  std::map<int, std::vector<size_t> >::iterator itbMap, itMap, iteMap;
  int idx = 0, k = 0;
  int sendBuffSize = 0;
  int nbSend = sendingIndexMap.size();
  std::vector<int> sendDispl(nbSend,0);
  itbMap = sendingIndexMap.begin();
  iteMap = sendingIndexMap.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap) sendBuffSize += (itMap->second).size();
  itMap = itbMap;
  for (idx = 1; idx < nbSend; ++itMap, ++idx) sendDispl[idx] = sendDispl[idx-1] + (itMap->second).size();

  double* sendBuff, *ptr;
  std::vector<MPI_Request> sendRequest(nbSend);
  if (0 != sendBuffSize) sendBuff = new double[sendBuffSize];
  for (k = 0, itMap = itbMap; itMap != iteMap; ++itMap, ++k)
  {
    int sendDataSize = (itMap->second).size();
    for (int i = 0; i < sendDataSize; ++i)
    {
      sendBuff[sendDispl[k]+i] = dataInput((itMap->second)[i]);
    }
    std::cout <<  "itMap first " << itMap->first << std::endl;
    ptr = sendBuff + sendDispl[k];
    MPI_Isend(ptr, sendDataSize, MPI_DOUBLE,
              itMap->first, 24, clientIntraComm, &sendRequest[k]);
  }

  MPI_Status status;
  for (idx = 0; idx < nbSend; ++idx) MPI_Wait(&sendRequest[idx], &status);

  int recvBuffSize = 0, recvBuffPos = 0;
  int nbRecv = receivingIndexMap.size();
  std::map<int,int> recvDispl;
  itbMap = receivingIndexMap.begin();
  iteMap = receivingIndexMap.end();
  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    recvBuffSize += (itMap->second).size();
    recvDispl[itMap->first] = recvBuffPos;
    recvBuffPos += (itMap->second).size();
  }

  double* recvBuff;
  std::map<int, MPI_Request> recvRequest;
  std::map<int, MPI_Request>::iterator itRequest;
  std::list<int> processedRequest;
  int nbRemainingRecv = nbRecv;
  if (0 != sendBuffSize) recvBuff = new double[recvBuffSize];

  for (itMap = itbMap; itMap != iteMap; ++itMap)
  {
    int rankSource = itMap->first;
    int recvDataSize = (itMap->second).size();
    MPI_Recv(recvBuff+recvDispl[rankSource], recvDataSize, MPI_DOUBLE,
             rankSource, 24, clientIntraComm, &status);
    for (int i = 0; i < recvDataSize; ++i)
    {
      dataOutput(receivingIndexMap[rankSource][i]) = recvBuff[recvDispl[rankSource]+recvDataSize-i-1];
    }
  }
  std::cout << "dataOutput 2 "<< dataOutput <<  std::endl;

//  while (0 < nbRemainingRecv)
//  {
//    MPI_Status status;
//    int flag, count;
//    MPI_Iprobe(MPI_ANY_SOURCE, 24, clientIntraComm, &flag, &status);
//    if ((true == flag))
//    {
//      MPI_Get_count(&status, MPI_DOUBLE, &count);
//      MPI_Irecv(recvBuff+recvDispl[status.MPI_SOURCE], count, MPI_DOUBLE,
//                status.MPI_SOURCE, 24, clientIntraComm,
//                &recvRequest[status.MPI_SOURCE]);
//
//    }
//    for (itRequest = recvRequest.begin(); itRequest != recvRequest.end(); ++itRequest)
//    {
//      int rank = itRequest->first;
//      processedRequest.push_back(rank);
//      int recvDataSize = (receivingIndexMap[rank]).size();
//      for (int i = 0; i < recvDataSize; ++i)
//      {
//        dataOutput(receivingIndexMap[rank][i]) = recvBuff[recvDispl[rank]+recvDataSize-i];
//      }
//      --nbRemainingRecv;
//    }
//
//    while (!processedRequest.empty())
//    {
//      recvRequest.erase(processedRequest.back());
//      processedRequest.pop_back();
//    }
//  }

  if (0 != sendBuffSize) delete [] sendBuff;
  if (0 != recvBuffSize) delete [] recvBuff;
}


void CInvertAlgorithm::inverseGlobalDataIndex(const CArray<size_t,1>& globalDataIndexInput)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;
  int nbClient = client->clientSize;
  int clientRank = client->clientRank;
  int idx = 0;

  int dataIndexSize = globalDataIndexInput.numElements();
  int* recvAxisSize = new int[nbClient];
  MPI_Allgather(&dataIndexSize, 1, MPI_INT,
                recvAxisSize, 1, MPI_INT, client->intraComm);

  std::vector<int> displ(nbClient);
  displ[0]=0 ;
  for(int n=1;n<nbClient;n++) displ[n]=displ[n-1]+recvAxisSize[n-1];
  int recvSize=displ[nbClient-1]+recvAxisSize[nbClient-1];
  unsigned long* recvBuff=new unsigned long[recvSize];

  unsigned long* sendBuff = new unsigned long[dataIndexSize];
  for (int idx = 0; idx < dataIndexSize; ++idx) sendBuff[idx] = (globalDataIndexInput)(idx);

  MPI_Allgatherv(sendBuff, dataIndexSize, MPI_UNSIGNED_LONG,
                 recvBuff, recvAxisSize, &displ[0], MPI_UNSIGNED_LONG,
                 client->intraComm);

  unsigned long* ptr;
  int receivingRank, sendingRank;
  std::vector<int>::iterator upper, itbDispl = displ.begin(), iteDispl = displ.end();

  //
//  std::map<int, std::vector<size_t> > sendingIndexMap;

  // map between client rank and (local) received data index
//  std::map<int, std::vector<size_t> > receivingIndexMap;
  std::vector<size_t> tmpIndexMap;
  for (idx = 0; idx < dataIndexSize; ++idx)
  {
    // First of all, need to know the inversed index
    size_t inversedIndex = recvSize - globalDataIndexInput(idx)-1;

    // Find position of the inversed index in the whole received array index
    ptr = std::find(recvBuff, recvBuff+recvSize, inversedIndex);
    if (ptr != recvBuff+recvSize)
    {
      int indexPosition = std::distance(recvBuff, ptr);
      // Determine a client from which the current client will receive data
      upper = std::upper_bound(itbDispl, iteDispl, indexPosition);
      sendingRank = receivingRank = std::distance(itbDispl, upper) - 1;
      if (clientRank != receivingRank)
      {
        (receivingIndexMap[receivingRank]).push_back(idx);
        (sendingIndexMap[sendingRank]).push_back(idx);
      }
      else
      {
        tmpIndexMap.push_back(idx);
      }
    }
  }
  if (!tmpIndexMap.empty())
  {
    int tmpSize = tmpIndexMap.size();
    for (int idx = 0; idx < tmpSize; ++idx)
    {
      indexMap[tmpIndexMap[idx]] = tmpIndexMap[tmpSize - idx -1];
    }
  }


  delete [] sendBuff;
  delete [] recvBuff;
  delete [] recvAxisSize;
}
}
