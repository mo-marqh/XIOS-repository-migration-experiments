/*!
   \file domain_algorithm_interpolate_from_file.hpp
   \author Ha NGUYEN
   \since 09 July 2015
   \date 09 Sep 2015

   \brief Algorithm for interpolation on a domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_HPP__
#define __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_HPP__

#include "domain_algorithm_transformation.hpp"
#include "transformation.hpp"

namespace xios {

class CDomain;
class CInterpolateDomain;

/*!
  \class CDomainAlgorithmInterpolate
  Reading interpolation from file then apply on a domain
*/
class CDomainAlgorithmInterpolate : public CDomainAlgorithmTransformation
{
public:
  CDomainAlgorithmInterpolate(CDomain* domainDestination, CDomain* domainSource, CInterpolateDomain* interpDomain);

  virtual ~CDomainAlgorithmInterpolate() {}

  static bool registerTrans();
protected:
  void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs);

private:
  void readInterpolationInfo(std::string& filename, std::map<int,std::vector<std::pair<int,double> > >& interpMapValue);
  void processPole(std::map<int,std::vector<std::pair<int,double> > >& interMapValuePole,
                   int nbGlobalPointOnPole);
  void computeRemap();
  void readRemapInfo();
  void exchangeRemapInfo(std::map<int,std::vector<std::pair<int,double> > >& interpMapValue);
private:
  CInterpolateDomain* interpDomain_;

private:

  static CGenericAlgorithmTransformation* create(CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CDomain>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
};

}
#endif // __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_HPP__
