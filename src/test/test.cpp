#include <iostream>
#include <sstream>
#include <string>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace std ;
using namespace boost::posix_time ;
using namespace boost::gregorian ;

int main(void)
{
      ptime t(time_from_string("2012-02-30 15:24")) ;
      
       
      std::cout << to_simple_string(t) << std::endl;

  return 1 ;  
}
