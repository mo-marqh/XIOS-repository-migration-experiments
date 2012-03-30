#include <iostream>
#include <sstream>
#include <string>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "calendar_type.hpp"
#include "date.hpp"
#include "calendar_util.hpp"

using namespace std ;
using namespace boost::posix_time ;
using namespace boost::gregorian ;
using namespace xios;
using namespace date ;

int main(void)
{
//      ptime t(time_from_string("2012-02-30 15:24")) ;
//      std::cout << to_simple_string(t) << std::endl;
      CGregorianCalendar MyCalendar("2011-03-01 00:00") ;
      cout<<MyCalendar.getInitDate()<<endl;
      cout<<MyCalendar.getCurrentDate()<<endl ;
      cout<<MyCalendar.getCurrentDate()-1*Day<<endl ;
  return 1 ;  
}
