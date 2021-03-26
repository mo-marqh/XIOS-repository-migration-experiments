#include "array_new.hpp"

namespace xios
{

#define macrotyperank(_TYPE_,_RANK_)\
  template<> int CArray<_TYPE_,_RANK_>::TV_ttf_display_type( const CArray<_TYPE_,_RANK_>* array ) \
  {\
    return show_TV_ttf_display_type (array) ;\
  }

#define macrotype(_TYPE_)\
macrotyperank(_TYPE_,1)\
macrotyperank(_TYPE_,2)\
macrotyperank(_TYPE_,3)\
macrotyperank(_TYPE_,4)\
macrotyperank(_TYPE_,5)\
macrotyperank(_TYPE_,6)\
macrotyperank(_TYPE_,7)

macrotype(double)
macrotype(int)
macrotype(bool)
macrotype(size_t)
macrotype(float)
macrotype(string)

#undef macrotyperank
#undef macrotype

}

