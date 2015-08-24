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

namespace xios {

CAxisAlgorithmInterpolate::CAxisAlgorithmInterpolate(CAxis* axisDestination, CAxis* axisSource, CInterpolateAxis* interpAxis)
: CAxisAlgorithmTransformation(axisDestination, axisSource)
{
  interpAxis->checkValid(axisSource);
  order_ = interpAxis->order.getValue();
  if (order_ >= axisSource->n_glo.getValue())
  {
    ERROR("CAxisAlgorithmInterpolate::CAxisAlgorithmInterpolate(CAxis* axisDestination, CAxis* axisSource, CInterpolateAxis* interpAxis)",
           << "Order of interpolation is greater than global size of axis source"
           << "Size of axis source " <<axisSource->getId() << " is " << axisSource->n_glo.getValue()  << std::endl
           << "Order of interpolation is " << order_ );
  }

  computeIndexSourceMapping();
}

/*!
  Compute the index mapping between axis on grid source and one on grid destination
*/
void CAxisAlgorithmInterpolate::computeIndexSourceMapping()
{
  CArray<double,1>& axisValue = axisSrc_->value;
  CArray<bool,1>& axisMask = axisSrc_->mask;

  CContext* context = CContext::getCurrent();
  CContextClient* client=context->client;
  int nbClient = client->clientSize;

  int srcSize  = axisSrc_->n_glo.getValue();
  int numValue = axisValue.numElements();

  std::vector<double> recvBuff(srcSize);
  std::vector<int> indexVec(srcSize);

  retrieveAllAxisValue(recvBuff, indexVec);
  order<double>(recvBuff, indexVec);
  computeInterpolantPoint(recvBuff, indexVec);
}

/*!
  Compute the interpolant points
  Assume that we have all value of axis source, with these values, need to calculate weight (coeff) of Lagrange polynomial
  \param [in] axisValue all value of axis source
  \param [in] indexVec permutation index of axisValue
*/
void CAxisAlgorithmInterpolate::computeInterpolantPoint(const std::vector<double>& axisValue, const std::vector<int>& indexVec)
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
  computeWeightedValueAndMapping(interpolatingIndexValues);
}

/*!
  Compute weight (coeff) of Lagrange's polynomial
  \param [in] interpolatingIndexValues the necessary axis value to calculate the coeffs
*/
void CAxisAlgorithmInterpolate::computeWeightedValueAndMapping(const std::map<int, std::vector<std::pair<int,double> > >& interpolatingIndexValues)
{
  std::map<int, std::vector<int> >& transMap = this->transformationMapping_;
  std::map<int, std::vector<double> >& transWeight = this->transformationWeight_;
  std::map<int, std::vector<std::pair<int,double> > >::const_iterator itb = interpolatingIndexValues.begin(), it,
                                                                      ite = interpolatingIndexValues.end();
  int ibegin = axisDest_->begin.getValue();
  for (it = itb; it != ite; ++it)
  {
    int globalIndexDest = it->first;
    double localValue = axisDest_->value(globalIndexDest - ibegin);
    const std::vector<std::pair<int,double> >& interpVal = it->second;
    int interpSize = interpVal.size();
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
      transMap[globalIndexDest].push_back(index);
      transWeight[globalIndexDest].push_back(weight);
    }
  }
}

/*!
  Each client retrieves all values of an axis
  \param [in/out] recvBuff buffer for receiving values (already allocated)
  \param [in/out] indexVec mapping between values and global index of axis
*/
void CAxisAlgorithmInterpolate::retrieveAllAxisValue(std::vector<double>& recvBuff, std::vector<int>& indexVec)
{
  CArray<double,1>& axisValue = axisSrc_->value;
  CArray<bool,1>& axisMask = axisSrc_->mask;

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

}
