/*!
   \file policy.cpp
   \author Ha NGUYEN
   \since 06 Oct 2015
   \date 06 Oct 2015

   \brief Some useful policies for templated classes
 */

#include "policy.hpp"
#include <cmath>

namespace xios
{
/*!
  Calculate MPI communicator for each level of hierarchy.
  \param[in] mpiCommRoot MPI communicator of the level 0 (usually communicator of all clients)
  \param[in] levels number of level in hierarchy
*/
void DivideCommByTwo::computeMPICommLevel(const MPI_Comm& mpiCommRoot, int levels)
{
  int nbProc;
  MPI_Comm_size(mpiCommRoot,&nbProc);
  if (levels > nbProc) levels = std::log10(nbProc) * 3.3219; // log2(x) = log2(10) * log10(x); stupid C++98
  else if (1 > levels) levels = 1;

  commLevel_.push_back(mpiCommRoot);
  divideMPICommLevel(mpiCommRoot, levels);
}

/*!
  Divide each MPI communicator into sub-communicator. Recursive function
  \param [in] mpiCommLevel MPI communicator of current level
  \param [in] level current level
*/
void DivideCommByTwo::divideMPICommLevel(const MPI_Comm& mpiCommLevel, int level)
{
  int clientRank;
  MPI_Comm_rank(mpiCommLevel,&clientRank);

   --level;
  if (0 < level)
  {
   int color = clientRank % 2;
   commLevel_.push_back(MPI_Comm());
   MPI_Comm_split(mpiCommLevel, color, 0, &(commLevel_.back()));
   divideMPICommLevel(commLevel_.back(), level);
  }
}

}

