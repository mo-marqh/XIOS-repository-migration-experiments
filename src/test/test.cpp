#include <iostream>
#include <sstream>
#include <string>

using namespace std ;


class CLog : public ostream
{
   public :
   CLog() : ostream(NULL),level(0) {}
   CLog& operator()(int l) {if (l<=level) rdbuf(cout.rdbuf()) ; else rdbuf(NULL) ; return *this;}
   void setLevel(int l) {level=l; } 
   int level ;
//   ostringstream& getStream() { return *(ostringstream*)(this) ; }
//   ~CLog(void) { cout<<"info message "<< this->str();}
};


int main(void)
{
  CLog out ;
  cout<<string("toto")<<endl ;
  out<<"123"<<endl ;
  out.setLevel(5) ;
  out(7)<<"coucou 1"<<endl ;
  out(3)<<"coucou 2"<<endl<<"i23"<<endl ;
  
}
