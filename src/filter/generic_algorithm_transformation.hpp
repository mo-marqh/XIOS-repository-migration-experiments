#ifndef __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__
#define __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__

#include <map>
#include <vector>
#include "array_new.hpp"

namespace xios {

class CGenericAlgorithmTransformation
{
public:
  CGenericAlgorithmTransformation();

  virtual ~CGenericAlgorithmTransformation() {}

  void computeGlobalSourceIndex(int elementPositionInGrid,
                                const std::vector<int>& gridDestGlobalDim,
                                const CArray<size_t,1>& globalIndexGridDestSendToServer,
                                std::map<size_t, std::set<size_t> >& globaIndexMapFromDestToSource);
protected:
  /*!
  Compute an array of global index from a global index on an element
    \param[in] destGlobalIndex global index on an element of destination grid
    \param[in] srcGlobalIndex global index(es) on an element of source grid (which are needed by one index on element destination)
    \param[in] elementPositionInGrid position of the element in the grid (for example: a grid with one domain and one axis, position of domain is 1, position of axis is 2)
    \param[in] gridDestGlobalDim dimension size of destination grid (it should share the same size for all dimension, maybe except the element on which transformation is performed)
    \param[in] globalIndexGridDestSendToServer global index of destination grid which are to be sent to server(s)
    \param[in/out] globalIndexDestGrid array of global index (for 2d grid, this array maybe a line, for 3d, this array may represent a plan). It should be preallocated
    \param[in/out] globalIndexSrcGrid array of global index of source grid (for 2d grid, this array is a line, for 3d, this array represents a plan). It should be preallocated
  */
  virtual void computeGlobalIndexFromGlobalIndexElement(int destGlobalIndex,
                                                        const std::vector<int>& srcGlobalIndex,
                                                        int elementPositionInGrid,
                                                        const std::vector<int>& gridDestGlobalDim,
                                                        const CArray<size_t,1>& globalIndexGridDestSendToServer,
                                                        CArray<size_t,1>& globalIndexDestGrid,
                                                        std::vector<CArray<size_t,1> >& globalIndexSrcGrid) = 0;

  /*!
  Compute global index mapping from one element of destination grid to the corresponding element of source grid
  */
  virtual void computeIndexSourceMapping() = 0;

protected:
  std::map<int, std::vector<int> > transformationMapping_;
};

}
#endif // __XIOS_GENERIC_ALGORITHM_TRANSFORMATION_HPP__
