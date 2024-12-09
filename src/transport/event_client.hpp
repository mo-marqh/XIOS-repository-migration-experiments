#ifndef __EVENT_CLIENT_HPP__
#define __EVENT_CLIENT_HPP__

#include "xios_spl.hpp"
#include "buffer_out.hpp"
#include "message.hpp"

namespace xios
{
  class CEventClient
  {
    public:
      static const size_t headerSize;

      CEventClient(int classId, int typeId);

      void push(int rank, int nbSender, CMessage& msg);
      void send(size_t timeLine, const std::list<int>& sizes, std::list<CBufferOut*>&); 
      bool isEmpty(void);
      std::list<int> getRanks(void);
      std::list<int> getSizes(void);
      int getClassId(void) { return classId; }
      int getTypeId(void) { return typeId; }
      
      void setFirst(void)
      {
        itRanks=ranks.begin() ;
        itNbSenders=nbSenders.begin() ;
        itMessages=messages.begin() ;
      }

      bool isFirst(void)
      {
        return itRanks==ranks.begin() ;
      }

      void next(void)
      {
        itRanks++ ;
        if (itRanks==ranks.end()) itRanks=ranks.begin() ;
        itNbSenders++ ;
        if (itNbSenders==nbSenders.end()) itNbSenders=nbSenders.begin() ;
        itMessages++ ;
        if (itMessages==messages.end()) itMessages=messages.begin() ;
      }
      
      void remove(void)
      {
        auto removedRank = itRanks;
        itRanks++ ;
        ranks.erase(removedRank) ;
        if (itRanks==ranks.end()) itRanks=ranks.begin() ;

        auto removedNbSender = itNbSenders ;
        itNbSenders++ ;
        nbSenders.erase(removedNbSender) ;
        if (itNbSenders==nbSenders.end()) itNbSenders=nbSenders.begin() ;

        auto removedMessage = itMessages ;
        itMessages++ ;
        messages.erase(removedMessage) ;
        if (itMessages==messages.end()) itMessages=messages.begin() ;
      }

      int getRank(void) { return *itRanks ;}
      int getNbSender(void) { return *itNbSenders ;}
      int getSize(void) { return (*itMessages)->size() + headerSize;}
      void send(size_t timeLine, int size, CBufferOut* buffer) ;
    private:
      int classId;
      int typeId;
      std::list<int> ranks;
      std::list<int> nbSenders;
      std::list<CMessage*> messages;
      
      std::list<int>::iterator       itRanks;
      std::list<int>::iterator       itNbSenders;
      std::list<CMessage*>::iterator itMessages;
  };
}

#endif
