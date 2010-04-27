#ifndef STDOUT_HPP
#define STDOUT_HPP

#include <fstream>

class xmlio_stdout : public std::ofstream
{
  public :
  
  static xmlio_stdout FirstInstance ;
  static xmlio_stdout & GetInstance(void) ;
  
  private :
  
  xmlio_stdout(void) ;
} ;

extern xmlio_stdout & StdOut ;

class xmlio_stderr : public std::ofstream
{
  public :
  
  static xmlio_stderr FirstInstance ;
  static xmlio_stderr & GetInstance(void) ;
  
  private :
  
  xmlio_stderr(void) ;
} ;
extern xmlio_stderr & StdErr ;

class xmlio_stdlog : public std::ofstream
{
  public :
  
  static xmlio_stdlog FirstInstance ;
  static xmlio_stdlog & GetInstance(void) ;
  
  private :
  
  xmlio_stdlog(void) ;
} ;
extern xmlio_stdlog & StdLog ;

#endif

