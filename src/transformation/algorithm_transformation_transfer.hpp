#ifndef __XIOS_ALGORITHM_TRANSFORMATION_TRANSFER_HPP__
#define __XIOS_ALGORITHM_TRANSFORMATION_TRANSFER_HPP__

#include "generic_algorithm_transformation.hpp"
#include "transfer_transform_connector.hpp"
#include "array_new.hpp"

namespace xios
{

  class CContext ;
  class CAlgorithmTransformationTransfer : public CGenericAlgorithmTransformation
  {
    public:

      CAlgorithmTransformationTransfer(CContext* context, bool isSource) : CGenericAlgorithmTransformation(context, isSource) {}
      virtual ~CAlgorithmTransformationTransfer() {};
      virtual void apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, CArray<double,1>& dataOut);
      virtual void computeRecvElement(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView) ;
      virtual StdString getAlgoName() {return "\\nCAlgorithm transformation Transfer";}
   
    protected:
      virtual void computeAlgorithm(shared_ptr<CLocalView> srcView, shared_ptr<CLocalView> dstView) ;

      //! Map between global index of destination element and source element
      unordered_map<int,int> transformationMapping_;
      shared_ptr<CTransferTransformConnector> transferTransformConnector_ ;
  };

}
#endif //__XIOS_ALGORITHM_TRANSFORMATION_TRANSFER_HPP__
