/*!
   \file policy.hpp
   \author Ha NGUYEN
   \since 06 Oct 2015
   \date 06 Oct 2015

   \brief Some useful policies for templated classes
 */

#ifndef __XIOS_POLICY_HPP__
#define __XIOS_POLICY_HPP__

#include <vector>
#include "mpi.hpp"

namespace xios
{
class DivideCommByTwo
{
protected:
  void computeMPICommLevel(const MPI_Comm& mpiCommRoot, int levels);

protected:
  std::vector<MPI_Comm> commLevel_;
private:
  // Divide MPI communicator on each level recursively
  void divideMPICommLevel(const MPI_Comm& mpiCommLevel, int level);
};

}

#endif // __XIOS_POLICY_HPP__
