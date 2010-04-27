#include <fstream>
#include "xmlio_std.hpp"


static const char file_stdout[]="xmlio.out" ;
static const char file_stderr[]="xmlio.err" ;
static const char file_stdlog[]="xmlio.log" ;


// declaration of singleton for standard output 

xmlio_stdout xmlio_stdout::FirstInstance ;
xmlio_stdout & StdOut=xmlio_stdout::GetInstance() ;

xmlio_stdout::xmlio_stdout(void) : std::ofstream(file_stdout)
{
  (*this)<<ResetIndent ;
}
  
xmlio_stdout & xmlio_stdout::GetInstance(void)
{
  return FirstInstance ;
}

// declaration of singleton for standard error 

xmlio_stderr xmlio_stderr::FirstInstance ;
xmlio_stderr & StdErr=xmlio_stderr::GetInstance() ;

xmlio_stderr::xmlio_stderr(void) : std::ofstream(file_stderr)
{
  (*this)<<ResetIndent ;
}
  
xmlio_stderr & xmlio_stderr::GetInstance(void)
{
  return FirstInstance ;
}
  

// declaration of singleton for standard error 

xmlio_stdlog xmlio_stdlog::FirstInstance ;
xmlio_stdlog & StdLog=xmlio_stdlog::GetInstance() ;

xmlio_stdlog::xmlio_stdlog(void) : std::ofstream(file_stdlog)
{
  (*this)<<ResetIndent ;
}
  
xmlio_stdlog & xmlio_stdlog::GetInstance(void)
{
  return FirstInstance ;
}
