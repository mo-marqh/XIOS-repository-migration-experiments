/*!
   \file axis_algorithm_inverse.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 09 June 2015

   \brief Algorithm for inversing an axis..
 */
#ifndef __XIOS_AXIS_ALGORITHM_INVERSE_HPP__
#define __XIOS_AXIS_ALGORITHM_INVERSE_HPP__

#include "axis_algorithm_transformation.hpp"
#include "axis.hpp"

namespace xios {
/*!
  \class CAxisAlgorithmInverse
  Inversing an axis
*/
class CAxisAlgorithmInverse : public CAxisAlgorithmTransformation
{
public:
  CAxisAlgorithmInverse(CAxis* axisDestination, CAxis* axisSource);

  virtual ~CAxisAlgorithmInverse() {}

  virtual void computeIndexSourceMapping();

private:
  void updateAxisValue();
};

}
#endif // __XIOS_AXIS_ALGORITHM_INVERSE_HPP__
