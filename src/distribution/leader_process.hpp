#ifndef __LEADER_PROCESS_HPP__
#define __LEADER_PROCESS_HPP__
#include <list>

namespace xios
{
  void computeLeaderProcess(int clientRank, int clientSize, int serverSize,
                                       std::list<int>& rankRecvLeader,
                                       std::list<int>& rankRecvNotLeader) ;
  int getLeaderRank(int clientSize, int serverSize, int serverRank) ;
}

#endif
