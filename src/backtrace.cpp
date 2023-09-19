#include <map>
#include <sstream>
#include <string>
#include "backtrace.hpp"
#include "cpptrace.hpp"

namespace xios
{
  namespace MemTrack
  {
    void backTrace(std::ostringstream& stack, int n)
    {
      using namespace cpptrace ;
      std::vector<stacktrace_frame> stackTrace ;
      stackTrace = generate_trace(n);
      for(auto& st : stackTrace)
      {
         size_t pos, pos1, pos2 ;
         pos1 = st.filename.rfind("/src/") ;
         if (pos1>st.filename.size()) pos1=0 ;
         pos2 = st.filename.rfind("/extern/") ;
         if (pos2>st.filename.size()) pos2=0 ;

         pos = pos1 > pos2 ? pos1 : pos2 ;

         std::string file = st.filename.substr(pos,st.filename.size()) ;
         stack<<st.symbol<<" ==> ."<<file <<" : l"<<st.line<<std::endl ;
      }
    }
  }
}