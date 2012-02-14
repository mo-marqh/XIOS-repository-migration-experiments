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

  template <typename T>
  T CXios::getin(const string& id, const T& defaultValue)
  {
    if (CObjectFactory::HasObject<CVariable>("xios",id))
      return CObjectFactory::GetObject<CVariable>("xios",id)->getData<T>() ;
    else return defaultValue ;
  }


}
#endif
