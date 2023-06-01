/*!
   \file algo_type.hpp
   \author Ha NGUYEN
   \since 28 Aug 2015
   \date 28 Aug 2015

   \brief A special transformation to generate a grid.
 */
#ifndef __XIOS_ALGORITHM_TRANSFORMATION_TYPES_HPP__
#define __XIOS_ALGORITHM_TRANSFORMATION_TYPES_HPP__

#include "scalar_algorithm_reduce_axis.hpp"
#include "scalar_algorithm_extract_axis.hpp"
#include "scalar_algorithm_reduce_domain.hpp"
#include "scalar_algorithm_reduce_scalar.hpp"
#include "scalar_algorithm_redistribute.hpp"

#include "axis_algorithm_inverse.hpp"
#include "axis_algorithm_zoom.hpp"
#include "axis_algorithm_interpolate.hpp"
#include "axis_algorithm_interpolate_coordinate.hpp"
#include "axis_algorithm_extract.hpp"
#include "axis_algorithm_reduce_domain.hpp"
#include "axis_algorithm_reduce_axis.hpp"
#include "axis_algorithm_extract_domain.hpp"
#include "axis_algorithm_temporal_splitting.hpp"
#include "axis_algorithm_duplicate_scalar.hpp"
#include "axis_algorithm_redistribute.hpp"

#include "domain_algorithm_zoom.hpp"
#include "domain_algorithm_interpolate.hpp"
#include "domain_algorithm_generate_rectilinear.hpp"
#include "domain_algorithm_compute_connectivity.hpp"
#include "domain_algorithm_expand.hpp"
#include "domain_algorithm_reorder.hpp"
#include "domain_algorithm_extract.hpp"
#include "domain_algorithm_redistribute.hpp"

namespace xios
{
  void registerAlgorithmTransformation(void) ;
}


#endif // __XIOS_ALGORITHM_TRANSFORMATION_TYPES_HPP__
