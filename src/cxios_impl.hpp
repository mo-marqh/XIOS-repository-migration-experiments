#ifndef __XIOS_IMPL_HPP__
#define __XIOS_IMPL_HPP__

#include "xmlioserver_spl.hpp"
#include "variable.hpp"

namespace xmlioserver
{
  template <typename T>
  T CXios::getin(const string& id)
  {
    return CObjectFactory::GetObject<CVariable>("xios",id)->getData<T>() ;
  }



}
#endif
