/*!
   \file domain_algorithm_generate_rectilinear.hpp
   \author Ha NGUYEN
   \since 31 Aug 2015
   \date 01 Sep 2015

   \brief Algorithm for automatic generation of rectilinear domain.
 */
#ifndef __XIOS_DOMAIN_ALGORITHM_GENERATE_RECTILINEAR_HPP__
#define __XIOS_DOMAIN_ALGORITHM_GENERATE_RECTILINEAR_HPP__

#include "algorithm_transformation_no_data_modification.hpp"
#include "transformation.hpp"

namespace xios {

class CGrid;
class CDomain;
class CGenerateRectilinearDomain;

/*!
  \class CDomainAlgorithmGenerateRectilinear
  Generate a rectilinear or CURVILINEAR domain and fill in necessary its attributes automatically
  A new rectilinear (or CURVILINEAR) domain will also be distributed automatically among the processes.
  The number of processes is deduced from the distribution of the grid source.
*/
class CDomainAlgorithmGenerateRectilinear : public CAlgorithmTransformationNoDataModification
{
public:
  CDomainAlgorithmGenerateRectilinear(bool isSource, CDomain* domainDestination, CDomain* domainSource,
                                      CGrid* gridDest, CGrid* gridSource,
                                      CGenerateRectilinearDomain* zoomDomain);

  virtual ~CDomainAlgorithmGenerateRectilinear() {}
  static bool registerTrans();
  virtual bool isGenerateTransformation(void) { return true ;}
  virtual StdString getAlgoName() {return "\\ngenerate_rectilinear_domain";}
protected:
  void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs);

private:
  void computeDistributionGridSource(CGrid* gridSrc);
  void computeDistributionGridDestination(CGrid* gridDest);
  void fillInAttributesDomainDestination();

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
private:
  int nbDomainDistributedPart_; //! Number of local domain.
  CDomain* domainDest_ ;

  static bool dummyRegistered_;
};

}
#endif // __XIOS_DOMAIN_ALGORITHM_GENERATE_RECTILINEAR_HPP__
