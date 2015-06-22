#ifndef __XIOS_INVERT_ALGORITHM_HPP__
#define __XIOS_INVERT_ALGORITHM_HPP__

#include "visitor.hpp"
#include "axis_transformation.hpp"
#include "axis_filter.hpp"

namespace xios {
class CInvertAlgorithm :
  public CBaseVisitor,
  public CVisitor<CAxisTransformation>,
  public CVisitor<CAxisFilter>
{
public:
  /** Default constructor */
  CInvertAlgorithm();

  void operate(CAxisTransformation&);
  void operate(CAxisFilter&);
protected:

  void inverseGlobalDataIndex(const CArray<size_t,1>& globalDataIndexInput);
protected:
  std::map<int, std::vector<size_t> > sendingIndexMap;
  std::map<int, std::vector<size_t> > receivingIndexMap;
  std::map<size_t, size_t> indexMap;
private:
};

}
#endif // __XIOS_INVERT_ALGORITHM_HPP__
