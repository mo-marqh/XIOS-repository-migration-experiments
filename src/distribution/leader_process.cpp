#include "leader_process.hpp"

namespace xios
{
  void computeLeaderProcess(int clientRank, int clientSize, int serverSize,
                                       std::list<int>& rankRecvLeader,
                                       std::list<int>& rankRecvNotLeader)
    {
      rankRecvLeader.clear() ;
      rankRecvNotLeader.clear() ;
      if ((0 == clientSize) || (0 == serverSize)) return;

      if (clientSize < serverSize)
      {
        int serverByClient = serverSize / clientSize;
        int remain = serverSize % clientSize;
        int rankStart = serverByClient * clientRank;

        if (clientRank < remain)
        {
          serverByClient++;
          rankStart += clientRank;
        }
        else rankStart += remain;

        for (int i = 0; i < serverByClient; i++)
          rankRecvLeader.push_back(rankStart + i);

        rankRecvNotLeader.resize(0);
      }
      else
      {
        int clientByServer = clientSize / serverSize;
        int remain = clientSize % serverSize;

        if (clientRank < (clientByServer + 1) * remain)
        {
          if (clientRank % (clientByServer + 1) == 0)
            rankRecvLeader.push_back(clientRank / (clientByServer + 1));
          else
            rankRecvNotLeader.push_back(clientRank / (clientByServer + 1));
        }
        else
        {
          int rank = clientRank - (clientByServer + 1) * remain;
          if (rank % clientByServer == 0)
            rankRecvLeader.push_back(remain + rank / clientByServer);
          else
            rankRecvNotLeader.push_back(remain + rank / clientByServer);
        }
      }
    }

  int getLeaderRank(int clientSize, int serverSize, int serverRank)
  {
    int rank ;
    if (clientSize > serverSize)
    {
      int serverByClient = clientSize / serverSize;
      int remain = clientSize % serverSize;
      rank=0 ;
      if (remain < serverRank) rank = (serverByClient+1)*remain + serverByClient * (serverRank-remain)  ;   
      else rank = (serverByClient+1) * serverRank ;
    }
    else
    {
      int serverByClient = serverSize / clientSize;
      int remain = serverSize % clientSize;
      
      if (remain*(serverByClient+1) > serverRank ) rank = serverRank/(serverByClient+1) ;
      else 
      {
        serverRank = serverRank-(serverByClient+1)*remain ;
        rank = remain + serverRank/serverByClient ;
      }
    }
    return rank ;
  }
}