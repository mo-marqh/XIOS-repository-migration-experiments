#include "algo_types.hpp"
#include <iostream>

  namespace xios
  {
    
    void registerAlgorithmTransformation(void)
    {
      static bool first = true ;
      if (!first) return ;
      bool ret ;
      std::cout<<"register Transformation"<<std::endl ;
      ret=CScalarAlgorithmReduceAxis::registerTrans();
      ret=CScalarAlgorithmExtractAxis::registerTrans();
      ret=CScalarAlgorithmReduceDomain::registerTrans();
      ret=CScalarAlgorithmReduceScalar::registerTrans();

      //! Axis
      ret=CAxisAlgorithmZoom::registerTrans();
      ret=CAxisAlgorithmExtractDomain::registerTrans();
      ret=CAxisAlgorithmInterpolate::registerTrans();
      ret=CAxisAlgorithmExtract::registerTrans();
      ret=CAxisAlgorithmInverse::registerTrans();
      ret=CAxisAlgorithmReduceDomain::registerTrans();
      ret=CAxisAlgorithmReduceAxis::registerTrans();
      ret=CAxisAlgorithmTemporalSplitting::registerTrans();
      ret=CAxisAlgorithmDuplicateScalar::registerTrans();

      //! Domain
      ret=CDomainAlgorithmComputeConnectivity::registerTrans();
      ret=CDomainAlgorithmInterpolate::registerTrans();
      ret=CDomainAlgorithmZoom::registerTrans();
      ret=CDomainAlgorithmExpand::registerTrans();
      ret=CDomainAlgorithmReorder::registerTrans();
      ret=CDomainAlgorithmExtract::registerTrans();
      first=false ;
    }
  }


