/*!
   \file domain_algorithm_compute_connectivity.hpp
   \author Ha NGUYEN
   \since 03 June 2015
   \date 12 June 2015

   \brief Algorithm for compute_connectivitying on an domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_COMPUTE_CONNECTIVITY_HPP__
#define __XIOS_DOMAIN_ALGORITHM_COMPUTE_CONNECTIVITY_HPP__

#include "algorithm_transformation_no_data_modification.hpp"
#include "transformation.hpp"

namespace xios {
class CDomain;
class CComputeConnectivityDomain;

/*!
  \class CDomainAlgorithmComputeConnectivity
*/
class CDomainAlgorithmComputeConnectivity : public CAlgorithmTransformationNoDataModification
{
public:
  CDomainAlgorithmComputeConnectivity(bool isSource, CDomain* domainDestination, CDomain* domainSource, CComputeConnectivityDomain* compute_connectivityDomain);

  virtual ~CDomainAlgorithmComputeConnectivity() {}

  static bool registerTrans();

protected:
  void computeLocalConnectivity(int type,
                                CDomain* domain,
                                int& nbConnectivityMax,
                                CArray<int,1>& nbConnectivity,
                                CArray<int,2>& localConnectivity);

public:
  static shared_ptr<CGenericAlgorithmTransformation> create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CDomain>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
  static bool dummyRegistered_;
};

}

#endif // __XIOS_DOMAIN_ALGORITHM_COMPUTE_CONNECTIVITY_HPP__