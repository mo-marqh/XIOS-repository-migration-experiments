#ifndef __XIOS_ZOOM_ALGORITHM_HPP__
#define __XIOS_ZOOM_ALGORITHM_HPP__

#include "visitor.hpp"
#include "axis_filter.hpp"

namespace xios {
class CZoomAlgorithm :
  public CBaseVisitor,
  public CVisitor<CAxisFilter>
{
public:
  /** Default constructor */
  CZoomAlgorithm();

  void operate(CAxisFilter&);
protected:
private:
};

}
#endif // __XIOS_ZOOM_ALGORITHM_HPP__
