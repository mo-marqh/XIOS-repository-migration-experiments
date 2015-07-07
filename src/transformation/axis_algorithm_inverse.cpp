/*!
   \file axis_algorithm_inverse.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 29 June 2015

   \brief Algorithm for inversing an axis..
 */
#include "axis_algorithm_inverse.hpp"
#include "transformation_mapping.hpp"
#include "context.hpp"
#include "context_client.hpp"

namespace xios {

CAxisAlgorithmInverse::CAxisAlgorithmInverse(CAxis* axisDestination, CAxis* axisSource)
 : CAxisAlgorithmTransformation(axisDestination, axisSource)
{
  if (axisDestination->size.getValue() != axisSource->size.getValue())
  {
    ERROR("CAxisAlgorithmInverse::CAxisAlgorithmInverse(CAxis* axisDestination, CAxis* axisSource)",
           << "Two axis have different size"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->size.getValue()  << std::endl
           << "Size of axis destionation " <<axisDestination->getId() << " is " << axisDestination->size.getValue());
  }

  this->computeIndexSourceMapping();
  int niSrc   = axisSrc_->ni.getValue();
  int sizeSrc = axisSrc_->size.getValue();
  if (niSrc != sizeSrc) updateAxisValue();
  else
  {
    for (int idx = 0; idx < sizeSrc; ++idx)
    {
      axisDest_->value(idx) = axisSrc_->value(sizeSrc-idx-1);
    }
  }
}

void CAxisAlgorithmInverse::computeIndexSourceMapping()
{
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_;

  int globalIndexSize = axisDestGlobalIndex_.size();
  for (int idx = 0; idx < globalIndexSize; ++idx)
  {
    transMap[axisDestGlobalIndex_[idx]].push_back(axisDestGlobalSize_-axisDestGlobalIndex_[idx]-1);
    transWeight[axisDestGlobalIndex_[idx]].push_back(1.0);
  }
}

/*!
  Update value on axis after inversing
  After an axis is inversed, not only the data on it must be inversed but also the value
*/
void CAxisAlgorithmInverse::updateAxisValue()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;

  int niSrc     = axisSrc_->ni.getValue();
  int ibeginSrc = axisSrc_->ibegin.getValue();

  CTransformationMapping transformationMap(axisDest_, axisSrc_);

  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_;

  std::map<size_t, std::vector<std::pair<size_t,double> > > globaIndexMapFromDestToSource;
  std::map<int, std::vector<int> >::const_iterator it = transMap.begin(), ite = transMap.end();
  for (; it != ite; ++it)
  {
    globaIndexMapFromDestToSource[it->first].push_back(make_pair((it->second)[0], (transWeight[it->first])[0]));
  }

  transformationMap.computeTransformationMapping(globaIndexMapFromDestToSource);

  const std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >& globalIndexToReceive = transformationMap.getGlobalIndexReceivedOnGridDestMapping();
  const std::map<int,std::vector<size_t> >& globalIndexToSend = transformationMap.getGlobalIndexSendToGridDestMapping();

 // Sending global index of original grid source
  std::map<int,std::vector<size_t> >::const_iterator itbSend = globalIndexToSend.begin(), itSend,
                                                     iteSend = globalIndexToSend.end();
 int sendBuffSize = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend) sendBuffSize += (itSend->second).size();

 typedef double Scalar;
 Scalar* sendBuff, *currentSendBuff;
 if (0 != sendBuffSize) sendBuff = new Scalar[sendBuffSize];
 for (StdSize idx = 0; idx < sendBuffSize; ++idx) sendBuff[idx] = NumTraits<Scalar>::sfmax();

 int currentBuffPosition = 0;
 for (itSend = itbSend; itSend != iteSend; ++itSend)
 {
   int destRank = itSend->first;
   const std::vector<size_t>& globalIndexOfCurrentGridSourceToSend = itSend->second;
   int countSize = globalIndexOfCurrentGridSourceToSend.size();
   for (int idx = 0; idx < (countSize); ++idx)
   {
     int index = globalIndexOfCurrentGridSourceToSend[idx] - ibeginSrc;
     sendBuff[idx+currentBuffPosition] = (axisSrc_->value)(index);
   }
   currentSendBuff = sendBuff + currentBuffPosition;
   MPI_Send(currentSendBuff, countSize, MPI_DOUBLE, destRank, 14, client->intraComm);
   currentBuffPosition += countSize;
 }

 // Receiving global index of grid source sending from current grid source
 std::map<int,std::vector<std::vector<std::pair<size_t,double> > > >::const_iterator itbRecv = globalIndexToReceive.begin(), itRecv,
                                                                                     iteRecv = globalIndexToReceive.end();
 int recvBuffSize = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv) recvBuffSize += (itRecv->second).size();

 Scalar* recvBuff, *currentRecvBuff;
 if (0 != recvBuffSize) recvBuff = new Scalar [recvBuffSize];
 for (StdSize idx = 0; idx < recvBuffSize; ++idx) recvBuff[idx] = NumTraits<Scalar>::sfmax();

 int currentRecvBuffPosition = 0;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   MPI_Status status;
   int srcRank = itRecv->first;
   int countSize = (itRecv->second).size();
   currentRecvBuff = recvBuff + currentRecvBuffPosition;
   MPI_Recv(currentRecvBuff, countSize, MPI_DOUBLE, srcRank, 14, client->intraComm, &status);
   currentRecvBuffPosition += countSize;
 }

 int ibeginDest = axisDest_->ibegin.getValue();
 currentRecvBuff = recvBuff;
 for (itRecv = itbRecv; itRecv != iteRecv; ++itRecv)
 {
   int countSize = (itRecv->second).size();
   for (int idx = 0; idx < countSize; ++idx, ++currentRecvBuff)
   {
     int ssize = (itRecv->second)[idx].size();
     for (int i = 0; i < ssize; ++i)
     {
       int index = ((itRecv->second)[idx][i]).first - ibeginDest;
       (axisDest_->value)(index) = *currentRecvBuff;
     }
   }
 }

 if (0 != sendBuffSize) delete [] sendBuff;
 if (0 != recvBuffSize) delete [] recvBuff;
}

}
