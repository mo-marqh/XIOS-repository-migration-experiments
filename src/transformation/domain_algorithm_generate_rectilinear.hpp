/*!
   \file domain_algorithm_generate_rectilinear.hpp
   \author Ha NGUYEN
   \since 31 Aug 2015
   \date 01 Sep 2015

   \brief Algorithm for automatic generation of rectilinear domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_GENERATE_RECTILINEAR_HPP__
#define __XIOS_DOMAIN_ALGORITHM_GENERATE_RECTILINEAR_HPP__

#include "domain_algorithm_transformation.hpp"

namespace xios {

class CGrid;
class CDomain;
class CGenerateRectilinearDomain;

/*!
  \class CDomainAlgorithmGenerateRectilinear
  Generate a rectilinear domain and fill in necessary its attributes automatically
  A new rectilinear domain will also be distributed automatically among the processes.
  The number of processes is deduced from the distribution of the grid source.
*/
class CDomainAlgorithmGenerateRectilinear : public CDomainAlgorithmTransformation
{
public:
  CDomainAlgorithmGenerateRectilinear(CDomain* domainDestination, CDomain* domainSource, CGrid* gridSource, CGenerateRectilinearDomain* zoomDomain);

  virtual ~CDomainAlgorithmGenerateRectilinear() {}

  virtual void computeIndexSourceMapping();

private:
  void computeDistributionGridSource();
  void fillInAttributesDomainDestination();

private:
  CGrid* gridSrc_;
  int nbDomainDistributedPart_; //! Number of local domain.

};

}
#endif // __XIOS_DOMAIN_ALGORITHM_GENERATE_RECTILINEAR_HPP__
