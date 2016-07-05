/*!
   \file scalar_algorithm_reduce_scalar.hpp
   \author Ha NGUYEN
   \since 23 June 2016
   \date 23 June 2016

   \brief Algorithm for reduce an axis to a scalar
 */
#ifndef __XIOS_SCALAR_ALGORITHM_REDUCE_AXIS_HPP__
#define __XIOS_SCALAR_ALGORITHM_REDUCE_AXIS_HPP__

#include "scalar_algorithm_transformation.hpp"

namespace xios {

class CScalar;
class CAxis;
class CReduceAxisToScalar;
class CReductionAlgorithm;

/*!
  \class CScalarAlgorithmReduceScalar
  Inversing an axis to a scalar
*/
class CScalarAlgorithmReduceScalar : public CScalarAlgorithmTransformation
{
public:
  CScalarAlgorithmReduceScalar(CScalar* scalarDestination, CAxis* axisSource, CReduceAxisToScalar* algo);

  virtual void apply(const std::vector<std::pair<int,double> >& localIndex,
                     const double* dataInput,
                     CArray<double,1>& dataOut,
                     std::vector<bool>& flagInitial);

  virtual ~CScalarAlgorithmReduceScalar();

protected:
  void computeIndexSourceMapping_(const std::vector<CArray<double,1>* >& dataAuxInputs);

protected:
  CReductionAlgorithm* reduction_;

};

}
#endif // __XIOS_SCALAR_ALGORITHM_REDUCE_AXIS_HPP__
