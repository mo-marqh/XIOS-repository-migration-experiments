#ifndef STDOUT_INDENT_HPP
#define STDOUT_INDENT_HPP
#include "xmlio_std.hpp"

extern int iostreamIndexIndent ;

ostream& iendl(ostream& o) ;
ostream& inc_endl(ostream& o) ;
ostream& dec_endl(ostream& o) ;

inline ostream& IncIndent(ostream& o)
{
  ++o.iword(iostreamIndexIndent) ;
  return o ;
}

inline ostream& DecIndent(ostream& o)
{
  if (o.iword(iostreamIndexIndent)>0)
    --o.iword(iostreamIndexIndent) ;
  return o ;
}

inline ostream& ResetIndent(ostream& o)
{
  o.iword(iostreamIndexIndent)=0 ;
  return o ;
}

inline ostream& iendl(ostream& o)
{
  o<<'\n' ;
  int mem=o.width(o.iword(iostreamIndexIndent)*2) ;
  o<<"";
  o.width(mem) ;
  return o ;
}

inline ostream& inc_endl(ostream& o)
{
  return o<<IncIndent<<iendl ;
}

inline ostream& dec_endl(ostream& o)
{
  return o<<DecIndent<<iendl ;
}


#endif




