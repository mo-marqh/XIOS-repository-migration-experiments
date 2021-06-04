/*!
   \file axis_algorithm_extract.hpp
   \brief Algorithm for extracting an axis.
 */
#ifndef __XIOS_AXIS_ALGORITHM_EXTRACT_HPP__
#define __XIOS_AXIS_ALGORITHM_EXTRACT_HPP__

#include "algorithm_transformation_transfer.hpp"
#include "transformation.hpp"

namespace xios {
class CAxis;
class CExtractAxis;

/*!
  \class CAxisAlgorithmExtract
  Implementing extract on axis
  A extracted region can be considered as region that isn't masked.
  Only this extracted region is extracted to write on Netcdf.
*/
class CAxisAlgorithmExtract : public CAlgorithmTransformationTransfer
{
public:
  CAxisAlgorithmExtract(bool isSource, CAxis* axisDestination, CAxis* axisSource, CExtractAxis* extractAxis);

  virtual ~CAxisAlgorithmExtract() {}

  static bool registerTrans();
  virtual StdString getAlgoName() {return "\\nextract_axis";}

private:

private:
  //! Global extract begin on axis
  StdSize extractBegin_;

  //! Global extract end on axis
  StdSize extractEnd_;

  //! Global extract size on axis
  StdSize extractN_;

  std::vector<int> extractIndex_;

private:
  CAxis* axisSrc_;
  CAxis* axisDest_;

public:
  static CGenericAlgorithmTransformation* create(bool isSource, CGrid* gridDst, CGrid* gridSrc,
                                                CTransformation<CAxis>* transformation,
                                                int elementPositionInGrid,
                                                std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                std::map<int, int>& elementPositionInGridDst2DomainPosition);
  static bool dummyRegistered_;
};

}
#endif // __XIOS_AXIS_ALGORITHM_EXTRACT_HPP__
