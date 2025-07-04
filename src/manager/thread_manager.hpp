#ifndef __THREAD_MANAGER_HPP__
#define __THREAD_MANAGER_HPP__

#include <thread>             // std::thread
#include <mutex>              // std::mutex, std::unique_lock
#include <condition_variable> // std::condition_variable
#include <map>


namespace xios
{
  class CThreadManager
  {
    private:
      
      struct SThreadInfo
      {
        std::thread* thread;
        std::thread::id previous;
        std::thread::id next;
        bool isNotified ;
        std::condition_variable* cvs ;
        std::unique_lock<std::mutex>* lck;
        bool finished ;
        bool isMainThread ;
      } ;

    
      static std::mutex* mtx_;
      static std::map<std::thread::id, SThreadInfo>* threads_;
      static std::thread::id masterThreadId_ ;
      static bool usingThreads_ ;
    
    public:
      
      static int getNumThreads(void) { return threads_->size()-1 ;}
      
      template <class Fn, class... Args> 
      static void spawnThread(Fn&& fn, Args&&... args)
      {
        std::thread::id myId=std::this_thread::get_id() ;

        SThreadInfo myThreadInfo ;
        myThreadInfo.thread=nullptr ;
        myThreadInfo.previous = myId ;
        myThreadInfo.next = (*threads_)[myId].next ;
        myThreadInfo.cvs = new std::condition_variable() ;
        myThreadInfo.thread = new std::thread(fn, args...) ;
        myThreadInfo.finished = false ;
        myThreadInfo.isNotified = false ;
        (*threads_)[myThreadInfo.thread->get_id()] =  myThreadInfo ;
        (*threads_)[(*threads_)[myId].next].previous = myThreadInfo.thread->get_id() ;
        (*threads_)[myId].next = myThreadInfo.thread->get_id() ;  

        do (*threads_)[myId].cvs->wait(*((*threads_)[myId].lck)) ;
        while(!(*threads_)[myId].isNotified) ;

        (*threads_)[myId].isNotified=false ;
      }

      static void yield(void)
      {
        if (isMasterThread()) checkJoin() ;
        std::thread::id id = std::this_thread::get_id() ;
        if (getNumThreads()>0)
        {
          (*threads_)[(*threads_)[id].next].isNotified=true ;
          (*threads_)[(*threads_)[id].next].cvs->notify_one() ;
          do (*threads_)[id].cvs->wait(*((*threads_)[id].lck)) ;
          while(!(*threads_)[id].isNotified) ;

          (*threads_)[id].isNotified=false ;

        }

      }
    
      static void checkJoin(void)
      {
        std::thread::id myId = std::this_thread::get_id() ;
        for(auto it=(*threads_).begin(); it!=(*threads_).end(); )
        { 
          if (it->first!=myId)
          {
            if (it->second.finished) 
            {
              it->second.thread->join() ;
              it=(*threads_).erase(it) ;
            }
            else 
            {
              it++ ;
            }
          }
            else it++ ;
        }
      }

      static bool isMasterThread(void) { return std::this_thread::get_id()==masterThreadId_; }
      static bool isUsingThreads(void) { return usingThreads_;}
      static void threadInitialize(void)
      {
        std::unique_lock<std::mutex>* lck = new std::unique_lock<std::mutex>(*mtx_);
        std::thread::id id=std::this_thread::get_id() ;

        (*threads_)[id].lck = lck ;
        (*threads_)[(*threads_)[id].previous].isNotified = true ;
        (*threads_)[(*threads_)[id].previous].cvs->notify_one() ;
        do (*threads_)[id].cvs->wait(*lck) ; while(!(*threads_)[id].isNotified) ;
        (*threads_)[id].isNotified=false ;
      }
    
      static void threadFinalize(void)
      {
        std::thread::id myId = std::this_thread::get_id() ;
        (*threads_)[(*threads_)[myId].previous].next = (*threads_)[myId].next;
        (*threads_)[(*threads_)[myId].next].previous = (*threads_)[myId].previous ;
        (*threads_)[myId].finished = true ;

        (*threads_)[(*threads_)[myId].next].isNotified=true ;
        (*threads_)[(*threads_)[myId].next].cvs->notify_one() ;
      
        delete (*threads_)[myId].lck ;
      }
   
      static void initialize(bool usingThreads)
      {
        mtx_ = new std::mutex ;
        threads_ = new std::map<std::thread::id, SThreadInfo> ;
        usingThreads_ = usingThreads ;

        std::thread::id myId= std::this_thread::get_id() ;
        SThreadInfo myThreadInfo ;
        myThreadInfo.thread=nullptr ;
        myThreadInfo.previous = myId ;
        myThreadInfo.next = myId ; 
        myThreadInfo.cvs = new std::condition_variable() ;
        myThreadInfo.lck = new std::unique_lock<std::mutex>(*mtx_) ;
        myThreadInfo.finished = false ;
        myThreadInfo.isNotified = false ;
        (*threads_)[myId] = myThreadInfo ;
      }
     
      static void finalize(void)
      {
        
        std::thread::id myId = std::this_thread::get_id() ;
        delete (*threads_)[myId].lck ;
        delete threads_ ;
        delete mtx_ ;
      }
  };

}


#endif
