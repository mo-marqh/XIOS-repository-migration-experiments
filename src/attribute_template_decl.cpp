#include "attribute_template_impl.hpp"
#include "attribute_template_specialisation.hpp"
#include <string>
#include "date.hpp"

namespace xios
{
  template class CAttributeTemplate<int> ;
  template class CAttributeTemplate<double> ;
  template class CAttributeTemplate<bool> ;
  template class CAttributeTemplate<string> ;
  template class CAttributeTemplate<CDate> ;
  template class CAttributeTemplate<CDuration> ;

#define macrotype(_TYPE_)						\
  template<> int CAttributeTemplate<_TYPE_>::TV_ttf_display_type( const CAttributeTemplate<_TYPE_>* attr ) \
  {\
    return show_TV_ttf_display_type (attr) ;\
  }

macrotype(double)
macrotype(int)
macrotype(bool)
macrotype(string)
//macrotype(CDate)
//macrotype(CDuration)
#undef macrotype

}
