#ifndef XMLIO_EXCEPTION_HPP
#define XMLIO_EXCEPTION_HPP

#include <sstream>
#include "xmlio_std.hpp"
using namespace std ;

class ex_error
{
  public :
  string what ;
  ex_error(const string & what_ ) : what(what_) {} ;
} ;

//class error;
//ostringstream & operator << (const error & err,char const * str) ;
//ostringstream & operator << (const error & err,const string & str) ;


class error : public ostringstream
{
  public :
    
    error(const string & where) : ostringstream()
    {
      *this<<"*** Error **** in function : "<<where<<endl<<"------> "<<string("toto") ;
    } ;
    
    error(const char * where) : ostringstream()
    {
      *this<<"Error in function : "<<where<<endl<<"------> " ;
    } ;

    ~error()
    {
      StdErr<< this->str() <<endl ;
      throw(ex_error(this->str())) ;
    } ;

   ostringstream & operator << (char const * str) const
   {
     ostringstream * tmp=const_cast <error *>(this) ;
     *tmp<<str ;
     return *tmp ;
   } ;

   ostringstream & operator << (const string& str) const
   {
     ostringstream * tmp=const_cast <error *>(this) ;
     *tmp<<str ;
     return *tmp ;
   } ; 

} ;

class warning : public ostringstream
{
  public :
    
    warning(const string & where) : ostringstream()
    {
      *this<<"*** Warning **** in function : "<<where<<endl<<"------> ";
    } ;
    
    warning(const char * where) : ostringstream()
    {
      *this<<"Error in function : "<<where<<endl<<"------> " ;
    } ;

    ~warning()
    {
      StdErr<< this->str() <<endl ;
    } ;

   ostringstream & operator << (char const * str) const
   {
     ostringstream * tmp=const_cast <warning *>(this) ;
     *tmp<<str ;
     return *tmp ;
   } ;

   ostringstream & operator << (const string& str) const
   {
     ostringstream * tmp=const_cast <warning *>(this) ;
     *tmp<<str ;
     return *tmp ;
   } ; 

} ;


#endif
