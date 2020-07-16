#ifndef __ELEMENT_VIEW_HPP__
#define __ELEMENT_VIEW_HPP__

#include "xios_spl.hpp"
#include "array_new.hpp"

namespace xios
{
  class CElementView
  {
    public:
      enum type : size_t { FULL=0, MODEL, WORKFLOW} ;
      const static int numViewType_ = 3 ;
    protected:
      type type_;
  } ;
 
}

#endif