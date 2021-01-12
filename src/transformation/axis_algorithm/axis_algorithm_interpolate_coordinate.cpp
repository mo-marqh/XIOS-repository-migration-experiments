/*!
   \file axis_algorithm_interpolate.cpp
   \author Ha NGUYEN
   \since 23 June 2015
   \date 02 Jul 2015

   \brief Algorithm for interpolation on an axis.
 */
#include "axis_algorithm_interpolate_coordinate.hpp"
#include "axis.hpp"
#include "interpolate_axis.hpp"
#include <algorithm>
#include <numeric>
#include "context.hpp"
#include "context_client.hpp"
#include "utils.hpp"
#include "grid.hpp"
#include "grid_transformation_factory_impl.hpp"
#include "distribution_client.hpp"
#include "transform_filter.hpp"
#include "timer.hpp"

namespace xios
{
  CGenericAlgorithmTransformation* CAxisAlgorithmInterpolateCoordinate::create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                                     CTransformation<CAxis>* transformation,
                                                                     int elementPositionInGrid,
                                                                     std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                                     std::map<int, int>& elementPositionInGridDst2DomainPosition)
  TRY
  {
    std::vector<CAxis*> axisListDestP = gridDst->getAxis();
    std::vector<CAxis*> axisListSrcP  = gridSrc->getAxis();

    CInterpolateAxis* interpolateAxis = dynamic_cast<CInterpolateAxis*> (transformation);
    int axisDstIndex = elementPositionInGridDst2AxisPosition[elementPositionInGrid];
    int axisSrcIndex = elementPositionInGridSrc2AxisPosition[elementPositionInGrid];

    return (new CAxisAlgorithmInterpolateCoordinate(isSource, axisListDestP[axisDstIndex], axisListSrcP[axisSrcIndex], interpolateAxis));
  }
  CATCH

  bool CAxisAlgorithmInterpolateCoordinate::dummyRegistered_ = CAxisAlgorithmInterpolateCoordinate::registerTrans();
  bool CAxisAlgorithmInterpolateCoordinate::registerTrans()
  TRY
  {
    return CGridTransformationFactory<CAxis>::registerTransformation(TRANS_INTERPOLATE_AXIS, create);
  }
  CATCH

  vector<string> CAxisAlgorithmInterpolateCoordinate::getAuxFieldId(void)
  {
    if (hasCoordinate_) return vector<string>(1,coordinate_) ;
    else return vector<string>() ;
  }

  CAxisAlgorithmInterpolateCoordinate::CAxisAlgorithmInterpolateCoordinate(bool isSource, CAxis* axisDestination, CAxis* axisSource, CInterpolateAxis* interpAxis)
  : CAlgorithmTransformationTransfer(isSource), axisSrc_(axisSource), axisDest_(axisDestination)
  TRY 
  {
    interpAxis->checkValid(axisSource);
    axisDestination->checkAttributes() ;

    order_ = interpAxis->order.getValue();
    if (!interpAxis->coordinate.isEmpty())
    {
      coordinate_ = interpAxis->coordinate.getValue();
      hasCoordinate_=true ;
    }
  
    ngloSrc_=axisSource->n_glo ;
    nDest_ =  axisDest_-> getLocalView(CElementView::WORKFLOW)->getSize() ;
    CArray<double,1> coord ;
    CLocalConnector destConnector(axisDest_->getLocalView(CElementView::FULL), axisDest_->getLocalView(CElementView::WORKFLOW)) ;
    destConnector.computeConnector() ;
    destConnector.transfer(axisDest_->value, coord) ;
    destCoordinate_ = vector<double>(coord.dataFirst(), coord.dataFirst()+nDest_) ;
    
    CArray<size_t,1> globalIndex(ngloSrc_) ;
    for(int i=0;i<ngloSrc_;i++) 
    {
      transformationMapping_[i] = i ;
      globalIndex(i) = i ;
    }
    

    CLocalElement axisSourceGlo(CContext::getCurrent()->getIntraCommRank(), ngloSrc_, globalIndex) ;
    axisSourceGlo.addFullView() ; 

    this->computeAlgorithm(axisSource->getLocalView(CElementView::WORKFLOW), axisSourceGlo.getView(CElementView::FULL)) ;
  }
  CATCH

  CTransformFilter* CAxisAlgorithmInterpolateCoordinate::createTransformFilter(CGarbageCollector& gc, CGridAlgorithm* algo, bool detectMissingValues, double defaultValue)
  {
    return new CTransformFilter(gc, 2, algo, detectMissingValues, defaultValue) ;  
  }

  void CAxisAlgorithmInterpolateCoordinate::apply(int dimBefore, int dimAfter, const CArray<double,1>& dataIn, 
                                                   const vector<CArray<double,1>>& auxDataIn, CArray<double,1>& dataOut)
  {
    CArray<double,1> dataInTmp;
    CArray<double,1> auxDataInTmp ;
    transferTransformConnector_ -> transfer(dimBefore, dimAfter, dataIn, dataInTmp) ;
    transferTransformConnector_ -> transfer(dimBefore, dimAfter, auxDataIn[0], auxDataInTmp) ;
  
    dataOut.resize(dimBefore*dimAfter*nDest_) ;
    const double* pressure = auxDataInTmp.dataFirst() ;
    const double* in = dataInTmp.dataFirst() ;
    double* out = dataOut.dataFirst() ;

    size_t sliceSrc = dimAfter*ngloSrc_ ;
    size_t sliceDest = dimAfter*nDest_ ;
    vector<double> srcCoordinate(ngloSrc_) ;
    vector<double> destCoordinate(nDest_) ;
    std::vector<int> srcIndex(ngloSrc_);
    vector<double> srcValue(ngloSrc_) ;
    vector<double> destValue(nDest_) ;
    std::vector<int> destIndex(nDest_);

    size_t nsrc ;
    for(size_t j=0, posJsrc=0, posJdest=0 ;  j<dimBefore ; j++, posJsrc+=sliceSrc, posJdest+=sliceDest )
      for(size_t k=0, posKsrc=posJsrc, posKdest=posJdest ; k<dimAfter ; k++, posKsrc++,posKdest++)
      {
        nsrc=0 ;
        for(size_t i=0, posIsrc=posKsrc, posIdest=posKdest ; i<ngloSrc_ ; i++, posIsrc+=dimAfter, posIdest+=dimAfter)
        {
          if ( !( std::isnan(pressure[posIsrc]) || std::isnan(in[posIsrc]) ) )
          {
            srcCoordinate[nsrc]=pressure[posIsrc] ;
            srcValue[nsrc] = in[posIsrc] ;
            nsrc++ ;
          }
        }
        destCoordinate = destCoordinate_ ;    
        computeInterp(nsrc, srcCoordinate, srcValue, srcIndex, nDest_, destCoordinate, destValue, destIndex) ;

        for(size_t i=0, posIdest=posKdest ; i<nDest_ ; i++, posIdest+=dimAfter)  out[posIdest] = destValue[i] ;
      }

  }

  void CAxisAlgorithmInterpolateCoordinate::computeInterp(int nsrc, vector<double>& srcCoordinate, vector<double>& srcValue, vector<int>& srcIndex,
                                                          int ndst, vector<double>& dstCoordinate, vector<double>& dstValue, vector<int>& dstIndex)
  {
    double x,y ;
    double d ;
  
    iota(srcIndex.data(), srcIndex.data()+nsrc, 0); // sort array and retrive sorted index
    stable_sort(srcIndex.data(), srcIndex.data()+nsrc, [&srcCoordinate](size_t i1, size_t i2) {return srcCoordinate[i1] < srcCoordinate[i2];});

    iota(dstIndex.data(), dstIndex.data()+ndst, 0);
    stable_sort(dstIndex.data(), dstIndex.data()+ndst, [&dstCoordinate](size_t i1, size_t i2) {return dstCoordinate[i1] < dstCoordinate[i2];});

    if (order_==1 || nsrc<=2)
    {
      if (nsrc<=1) dstValue.assign(ndst,std::numeric_limits<double>::quiet_NaN());
      else
      {

        double x0,x1 ;
        double y0,y1 ;
        int lastj=0;
      
        for(int i=0; i < ndst;i++)
        {
          x=dstCoordinate[dstIndex[i]] ;
          if ( x<=srcCoordinate[srcIndex[0]]) lastj=0 ;
          else if (x>=srcCoordinate[srcIndex[nsrc-1]]) lastj=nsrc-2 ;
          else
          {
            for(int j=lastj; j<nsrc; j++)
            { 
              if (x >= srcCoordinate[srcIndex[j]] && x<srcCoordinate[srcIndex[j+1]]) ;
              lastj=j ;
              break ;
            }  
          }
          x0=srcCoordinate[srcIndex[lastj]] ;
          x1=srcCoordinate[srcIndex[lastj+1]] ;
          y0=srcValue[srcIndex[lastj]] ;
          y1=srcValue[srcIndex[lastj+1]] ;
          y=((x-x1)*y0-(x-x0)*y1)/(x0-x1) ;
          dstValue[dstIndex[i]]=y ;
        }
      }
    }
    else if (order_==2)
    {
      double x0,x1,x2 ;
      double y0,y1,y2 ;
      int lastj=0, cj ;
      
      for(int i=0; i < ndst;i++)
      {
        x=dstCoordinate[dstIndex[i]] ;
        if ( x<=srcCoordinate[srcIndex[0]]) lastj=0 ;
        else if (x>=srcCoordinate[srcIndex[nsrc-1]]) lastj=nsrc-2 ;
        else
        {
          for(int j=lastj; j<nsrc; j++)
          { 
            if (x >= srcCoordinate[srcIndex[j]] && x<srcCoordinate[srcIndex[j+1]]) ;
            
            lastj=j ;
            break ;
          }  
        }
       
        if (lastj==0) cj=1 ;
        else if (lastj>=nsrc-2) cj=nsrc-2 ;
        else
        {
          if ( (x-srcCoordinate[srcIndex[lastj-1]]) > (srcCoordinate[srcIndex[lastj+2]]-x) ) cj=lastj ;
          else cj=lastj+1 ;
        } 
        x0=srcCoordinate[srcIndex[cj-1]] ;
        x1=srcCoordinate[srcIndex[cj]] ;
        x2=srcCoordinate[srcIndex[cj+1]] ;
        y0=srcValue[srcIndex[cj-1]] ;
        y1=srcValue[srcIndex[cj]] ;
        y2=srcValue[srcIndex[cj+1]] ;
            
        y=y0*(x-x1)*(x-x2)/((x0-x1)*(x0-x2)) + y1*(x-x0)*(x-x2)/((x1-x0)*(x1-x2)) + y2*(x-x0)*(x-x1)/((x2-x0)*(x2-x1))  ;
        dstValue[dstIndex[i]]=y ;
      }
    }
  }  

}