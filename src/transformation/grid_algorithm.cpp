#include "grid_algorithm.hpp"
#include "generic_algorithm_transformation.hpp"

namespace xios
{

  CTransformFilter* CGridAlgorithm::createTransformFilter(CGarbageCollector& gc, bool detectMissingValues, double defaultValue)
  {  
    return algorithm_->createTransformFilter(gc, shared_from_this(), detectMissingValues, defaultValue) ;
  }

  bool CGridAlgorithm::isGenerateTranformation(void)
  { 
    return algorithm_->isGenerateTransformation() ;
  } 


  StdString CGridAlgorithm::getAlgoName()
  {
    return algorithm_->getAlgoName();
  } 
}
