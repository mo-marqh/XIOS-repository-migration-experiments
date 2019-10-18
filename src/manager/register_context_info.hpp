#ifndef __CONTEXT_INFO_HPP__
#define __CONTEXT_INFO_HPP__

#include "services_manager.hpp"

namespace xios
{
  struct SRegisterContextInfo 
  {
    string poolId ;
    string serviceId;
    int partitionId ;
    int serviceType ;
    string id ;
    int size ;
    int leader ;
  } ;
}


#endif