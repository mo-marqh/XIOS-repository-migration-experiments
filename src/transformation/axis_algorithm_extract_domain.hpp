/*!
   \file axis_algorithm_reduce_domain.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a axis
 */
#ifndef __XIOS_AXIS_ALGORITHM_EXTRACT_DOMAIN_HPP__
#define __XIOS_AXIS_ALGORITHM_EXTRACT_DOMAIN_HPP__

#include "axis_algorithm_transformation.hpp"

namespace xios {

class CAxis;
class CDomain;
class CExtractDomainToAxis;
class CReductionAlgorithm;

/*!
  \class CAxisAlgorithmExtractDomain
  Extract a domain to an axis
*/
class CAxisAlgorithmExtractDomain : public CAxisAlgorithmTransformation
{
public:
  CAxisAlgorithmExtractDomain(CAxis* axisDestination, CDomain* domainSource, CExtractDomainToAxis* algo);

  virtual void apply(const std::vector<std::pair<int,double> >& localIndex,
                     const double* dataInput,
                     CArray<double,1>& dataOut,
                     std::vector<bool>& flagInitial);

  virtual ~CAxisAlgorithmExtractDomain();

protected:
  enum ExtractDirection {
    undefined = 0,
    iDir = 1,
    jDir = 2
  };

  ExtractDirection dir_;
  int pos_; //! Position to extract
protected:
  void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs);

protected:
  CReductionAlgorithm* reduction_;

};

}
#endif // __XIOS_AXIS_ALGORITHM_EXTRACT_DOMAIN_HPP__
