/*!
   \file reduction_types.hpp
   \author Ha NGUYEN
   \since 27 June 2016
   \date 27 June 2016

   \brief Different reduction types
 */
#ifndef __XIOS_REDUCTION_TYPES_HPP__
#define __XIOS_REDUCTION_TYPES_HPP__

namespace xios
{

  typedef enum reduction_algorithm_type
  {
    TRANS_REDUCE_NONE = 0, 
    TRANS_REDUCE_SUM = 1,
    TRANS_REDUCE_MIN = 2,
    TRANS_REDUCE_MAX = 3,
    TRANS_REDUCE_EXTRACT = 4,
    TRANS_REDUCE_AVERAGE = 5
  } EReductionType;

  enum class EReduction
  {
    none, sum, min, max, extract, average
  } ;
}
#endif // __XIOS_REDUCTION_TYPES_HPP__
