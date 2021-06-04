
#ifndef __XIOS_DOMAIN_ALGORITHM_EXTRACT_HPP__
#define __XIOS_DOMAIN_ALGORITHM_EXTRACT_HPP__

#include "transformation.hpp"
#include "algorithm_transformation_transfer.hpp"

namespace xios {

class CDomain;
class CExtractDomain;

/*!
  \class CDomainAlgorithmExtract
  Implementing extract (alternative zoom) on domain
*/
class CDomainAlgorithmExtract : public CAlgorithmTransformationTransfer
{
public:
  CDomainAlgorithmExtract(bool isSource, CDomain* domainDestination, CDomain* domainSource, CExtractDomain* extractDomain);

  virtual ~CDomainAlgorithmExtract() {}
  virtual StdString getAlgoName() {return "\\nextract_domain";}

  static bool registerTrans();

private:
  //! Global extract begin on domain
  int extractIBegin_;
  int extractJBegin_;

  //! Global extract end on domain
  int extractIEnd_;
  int extractJEnd_;

  //! Global extract size on domain
  int extractNi_;
  int extractNj_;
  
  CDomain* domainSrc_ ;
  CDomain* domainDest_ ;


public:
  static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
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
#endif // __XIOS_DOMAIN_ALGORITHM_EXTRACT_HPP__
