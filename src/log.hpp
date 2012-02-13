#ifndef __XIOS_LOG__

#include <iostream>

namespace xmlioserver
{
  using namespace std ;

  class CLog : public ostream
  {
    public :
    CLog(void) : ostream(cout.rdbuf()),level(0) {}
    CLog& operator()(int l) 
    {  
      if (l<=level) 
      {
        rdbuf(cout.rdbuf()) ;
        *this<<"-> info : " ;
      }
      else rdbuf(NULL) ;
      return *this;
    }
    void setLevel(int l) {level=l; } 
    int getLevel() {return level ;}
    bool isActive(void) { if (rdbuf()==NULL) return true ; else return false ;}
    bool isActive(int l) {if (l<=level) return true ; else return false ; }

    private :
    int level ;
  };

  extern CLog info;
}
#endif
