#include "thread_manager.hpp"

namespace xios
{
  std::mutex* CThreadManager::mtx_;
  std::map<std::thread::id, CThreadManager::SThreadInfo>* CThreadManager::threads_;
  std::thread::id CThreadManager::masterThreadId_ = std::this_thread::get_id() ;
  bool CThreadManager::usingThreads_=false ;
}