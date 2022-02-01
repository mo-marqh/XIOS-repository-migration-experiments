/*!
   \file generic_algorithm_transformation.hpp
   \author Ha NGUYEN
   \since 14 May 2015
   \date 21 Mars 2016

   \brief Interface for all transformation algorithms.
 */
#include "generic_algorithm_transformation.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "client_client_dht_template.hpp"
#include "utils.hpp"
#include "timer.hpp"
#include "mpi.hpp"
#include "transform_connector.hpp"
#include "weight_transform_connector.hpp"
#include "grid_algorithm_generic.hpp"
#include "transform_filter.hpp"

namespace xios 
{

CGenericAlgorithmTransformation::CGenericAlgorithmTransformation(bool isSource)
 : isSource_(isSource)
{
}



///////////////////////////////////////////////////////////////
////////// new algorithm for new method               /////////
///////////////////////////////////////////////////////////////

shared_ptr<CGridAlgorithm> CGenericAlgorithmTransformation::createGridAlgorithm(CGrid* gridSrc, CGrid* gridDst, int pos)
{
  auto algo = make_shared<CGridAlgorithmGeneric>(gridSrc, gridDst, pos, shared_from_this()) ;
  algo->computeAlgorithm() ;
  return algo ; 
}



CTransformFilter* CGenericAlgorithmTransformation::createTransformFilter(CGarbageCollector& gc, shared_ptr<CGridAlgorithm> algo, bool detectMissingValues, double defaultValue)
{
  return new CTransformFilter(gc, 1, algo, detectMissingValues, defaultValue) ;
}

vector<string> CGenericAlgorithmTransformation::getAuxFieldId(void) 
{
  return vector<string>() ;
}

}
