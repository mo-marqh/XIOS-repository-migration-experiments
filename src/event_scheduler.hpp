#ifndef __EVENT_SCHEDULER_HPP__
#define __EVENT_SCHEDULER_HPP__

#include "xios_spl.hpp"
#include "mpi.hpp"

namespace xios
{

    //!  Event scheduling class. An instance of this class is used to order the event providing from different context to avoid dead lock.
    /*!
     *   Event are ordered in a same context using the timeLine id, so each server will process the same event. But between different
     *   context, events are not scheduled and servers may choose to process different events and deadlock or MPI crash may occurs if
     *   collective MPI communication are involved by the events. 
     *   This class solve the problem by scheduling the event and choose which event must be process by each server to insure correct
     *   synchronisation. Information is send by asynchronous MPI communication to the root process that order the different events
     *   (First In First Out) and brodcast the information to the other servers. To avoid to much incoming communication for the root
     *   process, and hierachical tree is used for communicating from a limited number of child processes to the parent.  
     */
    
    class CEventScheduler
    {
       public:
       //!  Constructor
       /*! A new communicator is created by duplicate comm. The communicating tree hierarchy is created.
        *  @param[in] comm : MPI communicator du duplicate for internal use
        */
       CEventScheduler(const MPI_Comm& comm) ;
       CEventScheduler(const MPI_Comm& comm, size_t schedulerLevel) ;

       //! Destructor
       ~CEventScheduler() ;



       //! public interface for registring an event from the server
       /*!
        *  @param[in] timeLine : Time line id of the event
        *  @param[in] contextHashId : Hashed id of the context
        */
       void registerEvent(const size_t timeLine, const size_t contextHashId) ;
       
       private:
       CEventScheduler* getBaseScheduler(void) { if (childScheduler_== nullptr) return this; else return childScheduler_->getBaseScheduler();}

       public:
       //! public interface for query if the event defined by timeLine and hashId is sheduled next
       /*!
        *  @param[in] timeLine : Time line id of the event
        *  @param[in] contextHasId : Hashed id of the context
        *  @return  : boolean value, true is the event is scheduled next
        *
        *  If the event is scheduled next, it is remove from the `eventStack` queue list  
        */    
       bool queryEvent(const size_t timeLine, const size_t contextHashId) { return getBaseScheduler()->queryEvent_(timeLine, contextHashId); }
       bool queryEvent_(const size_t timeLine, const size_t contextHashId) ;
       void popEvent() { getBaseScheduler()->popEvent_() ; }
       void popEvent_() { eventStack_.pop() ; }
       bool isRoot(void) { return parent_[0]==mpiRank_ ;}
       void setParentScheduler(shared_ptr<CEventScheduler> parentScheduler) { parentScheduler_ = parentScheduler ;}
       void setChildScheduler(shared_ptr<CEventScheduler> childScheduler) { childScheduler_ = childScheduler ;}
       void splitScheduler(const MPI_Comm& splittedComm, shared_ptr<CEventScheduler>& parent, shared_ptr<CEventScheduler>& child) ;
       void cleanSplitSchedulers();

       //! Public interface to give the hand to the instance to check pending or incoming message.
       /*!
        * Must be called periodicaly. Call `checkParentRequest` and `checkChildRequest` private method.
        */
       void checkEvent(void) { getBaseScheduler()->checkEvent_(); } 
       void checkEvent_(void) ;

       private:
         void initialize(const MPI_Comm& comm) ;
       
       //! Send an event to the parent of level `lev+1`
       /*!
        *  @param[in] timeLine : Time line id of the event
        *  @param[in] contextHasId : Hashed id of the context
        *  @param[in] lev : actual level of the child in the hierarchy
        *  The event is sent by an asynchrounous MPI_ISend
        */
      
       void registerEvent(const size_t timeLine, const size_t contextHashId, const size_t schedulerLevel) ;
       void registerEvent(const size_t timeLine, const size_t contextHashId, const size_t schedulerLevel, const size_t lev) ;



       //! Children side. Check potential incoming message and if pending request are completed 
       /*!
        *  - Check by `MPI_Test` if pending request sent to parents are complete.
        *  - Probe incoming message from parent by using `MPI_Probe`. If yes, post an asynchronous reception by `MPI_IRecv`
        *  - Check by `MPI_Test` if pending received requests are complete. if yes :
        *    + Broadcast the event to the childrens if is also a parent
        *    + Otherwise : push the incomming event in the `eventStack` queue.
        */
       void checkParentRequest(void) ;



       //! Parent side. Check potential incoming message and if pending request are completed
       /*!
        *  - Probe incoming message from chidren by using `MPI_Probe`. If yes, post an asynchronous reception by `MPI_IRecv`.
        *  - Check pending received event request from children using `MPI_Probe`. If and event is received, it is incerted in the
        *    map `recvEvent` which is increased by 1. If the number of request received from children for this event is equal to the number
        *    of children then :
        *    + if the event level is 0, bcast the event to the children.
        *    + else send the event to the parent.
        *  - Check pending sent event request to children using `MPI_TEST` and if complete release the corresponding buffer 
        */
       void checkChildRequest(void) ;



       //! Parent side. Broadcast a received event from the parent to the children.
       /*!
        *  @param[in] timeLine : Time line id of the event
        *  @param[in] contextHasId : Hashed id of the context
        *  @param[in] lev : actual level of the child in the hierarchy
        * Asynchronus MPI_ISend is used.
        */
       void bcastEvent(const size_t timeLine, const size_t contextHashId, const size_t schedulerLevel ,const size_t lev) ;
       



       //! Structure defining an event, composed of the timeLine, the context hashId and the hierachical level of the communication.
       struct SEvent
       {
         size_t timeLine ; /*!< Time line id of the event in the context */
         size_t hashId ; /*!< hassh id of the context */
         size_t schedulerLevel ; /*!< hierarchical level of scherduler */
         size_t level ;  /*!<hierarchical level of the communication*/

         //! Definition of the == operator : needed to order the object in a map container
         /*!
            @param[in] e : object to compare with
            @return : boolean result of the comparison
         */
         bool operator==(const SEvent& e) const
         { 
           if (timeLine == e.timeLine && hashId == e.hashId && level==e.level && schedulerLevel==e.schedulerLevel) return true ;
           else return false ;
         } ;
        

         //! Definition of the < operator : needed to order the object in a map container
         /*!
            @param[in] e : object to compare with
            @return : boolean result of the comparison
         */

         bool operator<(const SEvent& e) const
         { 
           if (timeLine < e.timeLine) return true ;
           else if (timeLine == e.timeLine && hashId < e.hashId) return true ;
           else if (timeLine == e.timeLine && hashId == e.hashId && schedulerLevel<e.schedulerLevel) return true ;
           else if (timeLine == e.timeLine && hashId == e.hashId && schedulerLevel==e.schedulerLevel && level<e.level) return true ;
           else return false ;
         } ;
       } ;       
       
       //! Pending request struture. It keep send or receive buffer from asynchronous communication while the request is not complete.
       struct SPendingRequest
       {
         size_t buffer[4] ;      /*!< communication buffer : timeLine, hashId, level */
         MPI_Request request ;   /*!< pending MPI request */ 
       } ;
       
       MPI_Comm communicator_ ;  /*!< Internal MPI communicator */ 
       int mpiRank_ ;            /*!< Rank in the communicator */
       int mpiSize_ ;            /*!< Size of the communicator */
 
       queue< pair<size_t, size_t> > eventStack_ ;          
       queue<SPendingRequest* > pendingSentParentRequest_ ;   /*!< Pending request sent to parent   */
       queue<SPendingRequest*>  pendingRecvParentRequest_ ;   /*!< Pending request recv from parent */    
       list<SPendingRequest* >  pendingRecvChildRequest_ ;    /*!< Pending request recv from child  */
       list<SPendingRequest*>   pendingSentChildRequest_ ;    /*!< Pending request sent to child    */
       map< SEvent, int > recvEvent_ ;                        /*!< list of event received from children. Contains the currnet number children that have already post the same event */
       
       
       int level_ ;                   /*!< Number of hierachical level for communication */
       vector<int> parent_ ;          /*!< Parent rank for each level */ 
       vector<vector<int> >  child_ ; /*!< List of child rank for each level */
       vector<int> nbChild_ ;         /*!< Number of child for each level */    
       
       shared_ptr<CEventScheduler> parentScheduler_ ;
       shared_ptr<CEventScheduler> childScheduler_ ;
       bool hasParentScheduler_=false ;
       size_t schedulerLevel_ ;

    } ;
}

#endif
