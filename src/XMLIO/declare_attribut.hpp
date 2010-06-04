#ifndef __DECLARE_ATTRIBUT__
#define __DECLARE_ATTRIBUT__

#include "attribut.hpp"

#define DECLARE_ATTR(att_name,att_type)               \
   class attr_##att_name : public Attribut<att_type>  \
   {                                                  \
      public:                                         \
         const char * getName(void) const { return #att_name ;}                     \
         virtual const char * getType(void) const { return #att_type ;}             \
         attr_##att_name(void) : Attribut<att_type>() {}                            \
         attr_##att_name(const att_type& value_) : Attribut<att_type>(value_) {}    \
         attr_##att_name(const attr_##att_name & att) : Attribut<att_type>(att) {}  \
   } att_name

#endif // __DECLARE_ATTRIBUT__
