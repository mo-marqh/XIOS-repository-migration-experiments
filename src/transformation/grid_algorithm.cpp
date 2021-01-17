#include "grid_algorithm.hpp"
#include "generic_algorithm_transformation.hpp"

namespace xios
{

  CTransformFilter* CGridAlgorithm::createTransformFilter(CGarbageCollector& gc, bool detectMissingValues, double defaultValue)
  {  
    return algorithm_->createTransformFilter(gc, this, detectMissingValues, defaultValue) ;
  }

  bool CGridAlgorithm::isGenerateTranformation(void)
  { 
    return algorithm_->isGenerateTransformation() ;
  }  
}