#ifndef DECLARE_ATTRIBUT_HPP
#define DECLARE_ATTRIBUT_HPP
#include "attribut.hpp"

#define DECLARE_ATTR(att_name,att_type)                      \
  class attr_##att_name : public CAttribut<att_type>                         \
  {                                                          \
    public:                                                  \
    const char * getName(void) const { return #att_name ;} ; \
    attr_##att_name(void) : CAttribut<att_type>() {} ; \
    attr_##att_name(const att_type& value_) : CAttribut<att_type>(value_) {} ; \
    attr_##att_name(const attr_##att_name & att) : CAttribut<att_type>(att) {} ; \
  } att_name

#endif
