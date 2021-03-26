#include "attribute_array_impl.hpp"



namespace xios
{
  template class CAttributeArray<double,1> ;
  template class CAttributeArray<double,2> ;
  template class CAttributeArray<double,3> ;
  template class CAttributeArray<int,1> ;
  template class CAttributeArray<int,2> ;
  template class CAttributeArray<bool,1> ;
  template class CAttributeArray<bool,2> ;
  template class CAttributeArray<bool,3> ;
  template class CAttributeArray<bool,4> ;
  template class CAttributeArray<bool,5> ;
  template class CAttributeArray<bool,6> ;
  template class CAttributeArray<bool,7> ;
  template class CAttributeArray<StdString,1> ;
  template class CAttributeArray<StdString,2> ;

#define macrotyperank(_TYPE_,_RANK_)					\
  template<> int CAttributeArray<_TYPE_,_RANK_>::TV_ttf_display_type(const CAttributeArray<_TYPE_,_RANK_>* array ) \
  {\
    return CAttributeArray<_TYPE_,_RANK_>::show_TV_ttf_display_type (array) ;\
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
