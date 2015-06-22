#ifndef __XIOS_AXIS_FILTER_HPP__
#define __XIOS_AXIS_FILTER_HPP__

#include "generic_filter.hpp"
#include "axis.hpp"

namespace xios {

class CAxisFilter : public CGenericFilter
{
public:
  DEFINE_VISITABLE()
public:
  /** Default constructor */
  CAxisFilter(CGrid* gridInput, CGrid* gridOutput);

  const CArray<size_t,1>& getGlobalDataIndexInput() const;
  void setGridTransformed();
  bool isGridTransformed();

protected:
  bool isGridTransformed_;

};

}
#endif // __XIOS_AXIS_FILTER_HPP__
