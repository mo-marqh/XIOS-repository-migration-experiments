#ifndef FIELD_HPP
#define FIELD_HPP

#include "base_object.hpp"
#include "object_template.hpp"
#include "xmlio_std.hpp"


class Field : public CObjectTemplate<Field>, public CFieldAttribut
{
  public:

    Field(void) : CObjectTemplate<Field>() {} ;
    Field(const string & Id) : CObjectTemplate<Field>(Id) {} ;
    static const char * getName(void) { return "field" ; }
};


#endif
