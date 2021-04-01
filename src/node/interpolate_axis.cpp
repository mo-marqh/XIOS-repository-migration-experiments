#include "interpolate_axis.hpp"
#include "axis_algorithm_interpolate_coordinate.hpp"
#include "axis_algorithm_interpolate.hpp"
#include "type.hpp"
#include "field.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CInterpolateAxis::CInterpolateAxis(void)
    : CObjectTemplate<CInterpolateAxis>(), CInterpolateAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CInterpolateAxis::CInterpolateAxis(const StdString & id)
    : CObjectTemplate<CInterpolateAxis>(id), CInterpolateAxisAttributes(), CTransformation<CAxis>()
  { /* Ne rien faire de plus */ }

  CInterpolateAxis::~CInterpolateAxis(void)
  {}

  CTransformation<CAxis>* CInterpolateAxis::create(const StdString& id, xml::CXMLNode* node)
  {
    CInterpolateAxis* interpAxis = CInterpolateAxisGroup::get("interpolate_axis_definition")->createChild(id);
    if (node) interpAxis->parse(*node);
    return static_cast<CTransformation<CAxis>*>(interpAxis);
  }

  bool CInterpolateAxis::registerTrans()
  {
    return registerTransformation(TRANS_INTERPOLATE_AXIS, {create, getTransformation});
  }

  bool CInterpolateAxis::_dummyRegistered = CInterpolateAxis::registerTrans();

  //----------------------------------------------------------------

  StdString CInterpolateAxis::GetName(void)    { return StdString("interpolate_axis"); }
  StdString CInterpolateAxis::GetDefName(void) { return StdString("interpolate_axis"); }
  ENodeType CInterpolateAxis::GetType(void)    { return eInterpolateAxis; }

  void CInterpolateAxis::checkValid(CAxis* axisSrc)
  {
    if (this->order.isEmpty()) this->order.setValue(1);
    int order = this->order.getValue();
    if (order >= axisSrc->n_glo.getValue())
    {
      ERROR("CInterpolateAxis::checkValid(CAxis* axisSrc)",
             << "Order of interpolation is greater than global size of axis source"
             << "Size of axis source " <<axisSrc->getId() << " is " << axisSrc->n_glo.getValue()  << std::endl
             << "Order of interpolation is " << order );
    }

    if (order < 1)
    {
      ERROR("CInterpolateAxis::checkValid(CAxis* axisSrc)",
             << "Order of interpolation is smaller than 1"
             << "Size of axis source " <<axisSrc->getId() << " is " << axisSrc->n_glo.getValue()  << std::endl
             << "Order of interpolation is " << order );
    }

    if (!this->coordinate.isEmpty() && (!this->coordinate_src.isEmpty() || !this->coordinate_dst.isEmpty()))
    {
       ERROR("CInterpolateAxis::checkValid(CAxis* axisSrc)",
               << "<coordinate> is incompatible with <coordinate_src> or <coordinate_dst> attributes. <coordinate> attribute is present only for backward"
               << "compatibility and is now declared obsolete") ;
    }

    if (!this->coordinate.isEmpty())
    {
      if (!CField::has(coordinate))
        ERROR("CInterpolateAxis::checkValid(CAxis* axisSrc)",
               << "Coordinate field whose id " << coordinate << "does not exist "
               << "Please define one");
    }

    if (!this->coordinate_src.isEmpty())
    {
      if (!CField::has(coordinate_src))
        ERROR("CInterpolateAxis::checkValid(CAxis* axisSrc)",
               << "Coordinate field whose id " << coordinate_src << "does not exist "
               << "Please define one");
    }

    if (!this->coordinate_dst.isEmpty())
    {
      if (!CField::has(coordinate_dst))
        ERROR("CInterpolateAxis::checkValid(CAxis* axisSrc)",
               << "Coordinate field whose id " << coordinate_dst << "does not exist "
               << "Please define one");
    }
  }

  std::vector<StdString> CInterpolateAxis::checkAuxInputs_()
  {
    std::vector<StdString> auxInputs;
    
    if (!this->coordinate.isEmpty() && (!this->coordinate_src.isEmpty() || !this->coordinate_dst.isEmpty()))
    {
       ERROR("std::vector<StdString> CInterpolateAxis::checkAuxInputs_()",
               << "<coordinate> is incompatible with <coordinate_src> or <coordinate_dst> attributes. <coordinate> attribute is present only for backward"
               << "compatibility and is now declared obsolete") ;
    }

    if (!this->coordinate.isEmpty())
    {
      StdString coordinate = this->coordinate.getValue();
      if (!CField::has(coordinate))
        ERROR("std::vector<StdString> CInterpolateAxis::checkAuxInputs_()",
               << "Coordinate field whose id " << coordinate << "does not exist "
               << "Please define one");
      auxInputs.push_back(coordinate);
    }

    if (!this->coordinate_src.isEmpty())
    {
      StdString coordinate = this->coordinate_src.getValue();
      if (!CField::has(coordinate))
        ERROR("std::vector<StdString> CInterpolateAxis::checkAuxInputs_()",
               << "Coordinate field whose id " << coordinate << "does not exist "
               << "Please define one");
      auxInputs.push_back(coordinate);
    }

    if (!this->coordinate_dst.isEmpty())
    {
      StdString coordinate = this->coordinate_dst.getValue();
      if (!CField::has(coordinate))
        ERROR("std::vector<StdString> CInterpolateAxis::checkAuxInputs_()",
               << "Coordinate field whose id " << coordinate << "does not exist "
               << "Please define one");
      auxInputs.push_back(coordinate);
    }

    return auxInputs;
  }

  CGenericAlgorithmTransformation* CInterpolateAxis::createAlgorithm(bool isSource,
                                                        CGrid* gridDst, CGrid* gridSrc,
                                                        int elementPositionInGrid,
                                                        std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                        std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                        std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                        std::map<int, int>& elementPositionInGridDst2DomainPosition)
  {
    if (!coordinate.isEmpty() || !coordinate_src.isEmpty() || !coordinate_dst.isEmpty()) 
       return CAxisAlgorithmInterpolateCoordinate::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid, 
              elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
              elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
    
    else return CAxisAlgorithmInterpolate::create(isSource, gridDst,  gridSrc, this, elementPositionInGrid, 
                elementPositionInGridSrc2ScalarPosition, elementPositionInGridSrc2AxisPosition, elementPositionInGridSrc2DomainPosition,
                elementPositionInGridDst2ScalarPosition, elementPositionInGridDst2AxisPosition, elementPositionInGridDst2DomainPosition);
  }
}
