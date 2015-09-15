/*!
   \file domain_algorithm_interpolate_from_file.hpp
   \author Ha NGUYEN
   \since 09 July 2015
   \date 09 Sep 2015

   \brief Algorithm for interpolation on a domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_FROM_FILE_HPP__
#define __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_FROM_FILE_HPP__

#include "domain_algorithm_transformation.hpp"
#include "domain.hpp"
#include "interpolate_from_file_domain.hpp"

namespace xios {
/*!
  \class CDomainAlgorithmInterpolateFromFile
  Reading interpolation from file then apply on a domain
*/
class CDomainAlgorithmInterpolateFromFile : public CDomainAlgorithmTransformation
{
public:
  CDomainAlgorithmInterpolateFromFile(CDomain* domainDestination, CDomain* domainSource, CInterpolateFromFileDomain* interpDomain);

  virtual ~CDomainAlgorithmInterpolateFromFile() {}

  virtual void computeIndexSourceMapping();

private:
  void readInterpolationInfo(std::string& filename, std::map<int,std::vector<std::pair<int,double> > >& interpMapValue);
  void computeRemap();
  void readRemapInfo();

private:
  CInterpolateFromFileDomain* interpDomain_;

};

}
#endif // __XIOS_DOMAIN_ALGORITHM_INTERPOLATE_FROM_FILE_HPP__
