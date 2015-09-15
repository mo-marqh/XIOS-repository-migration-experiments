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
#include "domain.hpp"
#include "interpolate_domain.hpp"

namespace xios {
/*!
  \class CDomainAlgorithmInterpolate
  Reading interpolation from file then apply on a domain
*/
class CDomainAlgorithmInterpolate : public CDomainAlgorithmTransformation
{
public:
  CDomainAlgorithmInterpolate(CDomain* domainDestination, CDomain* domainSource, CInterpolateDomain* interpDomain);

  virtual ~CDomainAlgorithmInterpolate() {}

  virtual void computeIndexSourceMapping();

private:
  void readInterpolationInfo(std::string& filename, std::map<int,std::vector<std::pair<int,double> > >& interpMapValue);
  void computeRemap();
  void readRemapInfo();

private:
  CInterpolateDomain* interpDomain_;

};

}
#endif // __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_HPP__
