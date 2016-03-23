/*!
   \file axis_algorithm_interpolate.cpp
   \author Ha NGUYEN
   \since 23 June 2015
   \date 02 Jul 2015

   \brief Algorithm for interpolation on an axis.
 */
#include "axis_algorithm_interpolate.hpp"
#include <algorithm>
#include "context.hpp"
#include "context_client.hpp"
#include "utils.hpp"
#include "grid.hpp"
#include "distribution_client.hpp"

namespace xios {

CAxisAlgorithmInterpolate::CAxisAlgorithmInterpolate(CAxis* axisDestination, CAxis* axisSource, CInterpolateAxis* interpAxis)
: CAxisAlgorithmTransformation(axisDestination, axisSource), coordinate_(), transPosition_()
{
  interpAxis->checkValid(axisSource);
  order_ = interpAxis->order.getValue();
  if (!interpAxis->coordinate.isEmpty())
  {
    coordinate_ = interpAxis->coordinate.getValue();
    this->idAuxInputs_.resize(1);
    this->idAuxInputs_[0] = coordinate_;
  }

//  computeIndexSourceMapping();
}

/*!
  Compute the index mapping between axis on grid source and one on grid destination
*/
void CAxisAlgorithmInterpolate::computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int nbClient = client->clientSize;
  CArray<bool,1>& axisMask = axisSrc_->mask;
  int srcSize  = axisSrc_->n_glo.getValue();
  std::vector<CArray<double,1> > vecAxisValue;

  // Fill in axis value from coordinate
  fillInAxisValue(vecAxisValue, dataAuxInputs);

  for (int idx = 0; idx < vecAxisValue.size(); ++idx)
  {
    CArray<double,1>& axisValue = vecAxisValue[idx];
    std::vector<double> recvBuff(srcSize);
    std::vector<int> indexVec(srcSize);
    retrieveAllAxisValue(axisValue, axisMask, recvBuff, indexVec);
    XIOSAlgorithms::sortWithIndex<double, CVectorStorage>(recvBuff, indexVec);
    computeInterpolantPoint(recvBuff, indexVec, idx);
  }
}

/*!
  Compute the interpolant points
  Assume that we have all value of axis source, with these values, need to calculate weight (coeff) of Lagrange polynomial
  \param [in] axisValue all value of axis source
  \param [in] indexVec permutation index of axisValue
*/
void CAxisAlgorithmInterpolate::computeInterpolantPoint(const std::vector<double>& axisValue, const std::vector<int>& indexVec, int transPos)
{
  std::vector<double>::const_iterator itb = axisValue.begin(), ite = axisValue.end();
  std::vector<double>::const_iterator itLowerBound, itUpperBound, it;
  std::vector<int>::const_iterator itbVec = indexVec.begin(), itVec;
  const double sfmax = NumTraits<double>::sfmax();

  int ibegin = axisDest_->begin.getValue();
  CArray<double,1>& axisDestValue = axisDest_->value;
  int numValue = axisDestValue.numElements();
  std::map<int, std::vector<std::pair<int,double> > > interpolatingIndexValues;

  for (int idx = 0; idx < numValue; ++idx)
  {
    double destValue = axisDestValue(idx);
    itLowerBound = std::lower_bound(itb, ite, destValue);
    itUpperBound = std::upper_bound(itb, ite, destValue);
    if ((ite != itUpperBound) && (sfmax == *itUpperBound)) itUpperBound = ite;

    // If the value is not in the range, that means we'll do extra-polation
    if (ite == itLowerBound) // extra-polation
    {
      itLowerBound = itb;
      itUpperBound = itb + order_+1;
    }
    else if (ite == itUpperBound) // extra-polation
    {
      itLowerBound = itUpperBound - order_-1;
    }
    else
    {
      if (itb != itLowerBound) --itLowerBound;
      if (ite != itUpperBound) ++itUpperBound;
      int order = (order_ + 1) - 2;
      bool down = true;
      for (int k = 0; k < order; ++k)
      {
        if ((itb != itLowerBound) && down)
        {
          --itLowerBound;
          down = false;
          continue;
        }
        if ((ite != itUpperBound) && (sfmax != *itUpperBound))
        {
          ++itUpperBound;
          down = true;
        }
      }
    }

    for (it = itLowerBound; it != itUpperBound; ++it)
    {
      int index = std::distance(itb, it);
      interpolatingIndexValues[idx+ibegin].push_back(make_pair(indexVec[index],*it));
    }
  }
  computeWeightedValueAndMapping(interpolatingIndexValues, transPos);
}

/*!
  Compute weight (coeff) of Lagrange's polynomial
  \param [in] interpolatingIndexValues the necessary axis value to calculate the coeffs
*/
void CAxisAlgorithmInterpolate::computeWeightedValueAndMapping(const std::map<int, std::vector<std::pair<int,double> > >& interpolatingIndexValues, int transPos)
{
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_[transPos];
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_[transPos];
  std::map<int, std::vector<std::pair<int,double> > >::const_iterator itb = interpolatingIndexValues.begin(), it,
                                                                      ite = interpolatingIndexValues.end();
  int ibegin = axisDest_->begin.getValue();
  for (it = itb; it != ite; ++it)
  {
    int globalIndexDest = it->first;
    double localValue = axisDest_->value(globalIndexDest - ibegin);
    const std::vector<std::pair<int,double> >& interpVal = it->second;
    int interpSize = interpVal.size();
    transMap[globalIndexDest].resize(interpSize);
    transWeight[globalIndexDest].resize(interpSize);
    for (int idx = 0; idx < interpSize; ++idx)
    {
      int index = interpVal[idx].first;
      double weight = 1.0;

      for (int k = 0; k < interpSize; ++k)
      {
        if (k == idx) continue;
        weight *= (localValue - interpVal[k].second);
        weight /= (interpVal[idx].second - interpVal[k].second);
      }
      transMap[globalIndexDest][idx] = index;
      transWeight[globalIndexDest][idx] = weight;
      if (!transPosition_.empty())
      {
        (this->transformationPosition_[transPos])[globalIndexDest] = transPosition_[transPos];
      }
    }
  }
}

/*!
  Each client retrieves all values of an axis
  \param [in/out] recvBuff buffer for receiving values (already allocated)
  \param [in/out] indexVec mapping between values and global index of axis
*/
void CAxisAlgorithmInterpolate::retrieveAllAxisValue(const CArray<double,1>& axisValue, const CArray<bool,1>& axisMask,
                                                     std::vector<double>& recvBuff, std::vector<int>& indexVec)
{
  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int nbClient = client->clientSize;

  int srcSize  = axisSrc_->n_glo.getValue();
  int numValue = axisValue.numElements();

  if (srcSize == numValue)  // Only one client or axis not distributed
  {
    for (int idx = 0; idx < srcSize; ++idx)
    {
      if (axisMask(idx))
      {
        recvBuff[idx] = axisValue(idx);
        indexVec[idx] = idx;
      }
      else recvBuff[idx] = NumTraits<double>::sfmax();
    }

  }
  else // Axis distributed
  {
    double* sendValueBuff = new double [numValue];
    int* sendIndexBuff = new int [numValue];
    int* recvIndexBuff = new int [srcSize];

    int ibegin = axisSrc_->begin.getValue();
    for (int idx = 0; idx < numValue; ++idx)
    {
      if (axisMask(idx))
      {
        sendValueBuff[idx] = axisValue(idx);
        sendIndexBuff[idx] = idx + ibegin;
      }
      else
      {
        sendValueBuff[idx] = NumTraits<double>::sfmax();
        sendIndexBuff[idx] = -1;
      }
    }

    int* recvCount=new int[nbClient];
    MPI_Allgather(&numValue,1,MPI_INT,recvCount,1,MPI_INT,client->intraComm);

    int* displ=new int[nbClient];
    displ[0]=0 ;
    for(int n=1;n<nbClient;n++) displ[n]=displ[n-1]+recvCount[n-1];

    // Each client have enough global info of axis
    MPI_Allgatherv(sendIndexBuff,numValue,MPI_INT,recvIndexBuff,recvCount,displ,MPI_INT,client->intraComm);
    MPI_Allgatherv(sendValueBuff,numValue,MPI_DOUBLE,&(recvBuff[0]),recvCount,displ,MPI_DOUBLE,client->intraComm);

    for (int idx = 0; idx < srcSize; ++idx)
    {
      indexVec[idx] = recvIndexBuff[idx];
    }

    delete [] displ;
    delete [] recvCount;
    delete [] recvIndexBuff;
    delete [] sendIndexBuff;
    delete [] sendValueBuff;
  }
}

/*!
  Fill in axis value dynamically from a field whose grid is composed of a domain and an axis
  \param [in/out] vecAxisValue vector axis value filled in from input field
*/
void CAxisAlgorithmInterpolate::fillInAxisValue(std::vector<CArray<double,1> >& vecAxisValue,
                                                const std::vector<CArray<double,1>* >& dataAuxInputs)
{
  if (coordinate_.empty())
  {
    vecAxisValue.resize(1);
    vecAxisValue[0].resize(axisSrc_->value.numElements());
    vecAxisValue[0] = axisSrc_->value;
    this->transformationMapping_.resize(1);
    this->transformationWeight_.resize(1);
  }
  else
  {
    CField* field = CField::get(coordinate_);
    CGrid* grid = field->grid;

    std::vector<CDomain*> domListP = grid->getDomains();
    std::vector<CAxis*> axisListP = grid->getAxis();
    if (domListP.empty() || axisListP.empty() || (1 < domListP.size()) || (1 < axisListP.size()))
      ERROR("CAxisAlgorithmInterpolate::fillInAxisValue(std::vector<CArray<double,1> >& vecAxisValue)",
             << "XIOS only supports dynamic interpolation with coordinate (field) associated with grid composed of a domain and an axis"
             << "Coordinate (field) id = " <<field->getId() << std::endl
             << "Associated grid id = " << grid->getId());

    CDomain* dom = domListP[0];
    size_t vecAxisValueSize = dom->i_index.numElements();
    vecAxisValue.resize(vecAxisValueSize);
    if (transPosition_.empty())
    {
      transPosition_.resize(vecAxisValueSize);
      for (size_t idx = 0; idx < vecAxisValueSize; ++idx)
      {
        transPosition_[idx].resize(2);
        transPosition_[idx][0] = (dom->i_index)(idx);
        transPosition_[idx][1] = (dom->j_index)(idx);
      }
    }
    this->transformationMapping_.resize(vecAxisValueSize);
    this->transformationWeight_.resize(vecAxisValueSize);
    this->transformationPosition_.resize(vecAxisValueSize);

    const std::vector<size_t>& globalIndexSendToServer = grid->getDistributionClient()->getGlobalDataIndexSendToServer();
    const std::vector<int>& localDataSendToServer = grid->getDistributionClient()->getLocalDataIndexSendToServer();
    std::vector<int> permutIndex(globalIndexSendToServer.size());
    std::vector<int>::iterator itVec;
    XIOSAlgorithms::fillInIndex(globalIndexSendToServer.size(), permutIndex);
    XIOSAlgorithms::sortWithIndex<size_t, CVectorStorage>(globalIndexSendToServer, permutIndex);
    size_t axisSrcSize = axisSrc_->index.numElements();
    std::vector<int> globalDimension = grid->getGlobalDimension();
    XIOSBinarySearchWithIndex<size_t> binSearch(globalIndexSendToServer);
    for (size_t idx = 0; idx < vecAxisValueSize; ++idx)
    {
      size_t axisValueSize = 0;
      for (size_t jdx = 0; jdx < axisSrcSize; ++jdx)
      {
        size_t globalIndex = ((dom->i_index)(idx) + (dom->j_index)(idx)*globalDimension[0]) + (axisSrc_->index)(jdx)*globalDimension[0]*globalDimension[1];
        if (binSearch.search(permutIndex.begin(), permutIndex.end(), globalIndex, itVec))
        {
          ++axisValueSize;
        }
      }

      vecAxisValue[idx].resize(axisValueSize);
      axisValueSize = 0;
      for (size_t jdx = 0; jdx < axisSrcSize; ++jdx)
      {
        size_t globalIndex = ((dom->i_index)(idx) + (dom->j_index)(idx)*globalDimension[0]) + (axisSrc_->index)(jdx)*globalDimension[0]*globalDimension[1];
        if (binSearch.search(permutIndex.begin(), permutIndex.end(), globalIndex, itVec))
        {
          vecAxisValue[idx](axisValueSize) = (*dataAuxInputs[0])(localDataSendToServer[*itVec]);
          ++axisValueSize;
        }
      }
    }
  }
}

}
