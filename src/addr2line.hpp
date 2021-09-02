#ifdef XIOS_MEMTRACK

#ifndef __ADDR2LINE_HPP__
#define __ADDR2LINE_HPP__

#include <sys/types.h>
#include<sys/wait.h>
#include<sys/prctl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <string>
#include <iostream>
#include <fstream>

namespace xios
{
  class CAddr2line
  {
    public:

      CAddr2line(void) ;
      ~CAddr2line() ;

      void write(const std::string& str) ;
      void read(std::string& str) ;
   
    private:
      pid_t child_pid;
      int   from_child, to_child;
  } ;


  CAddr2line::CAddr2line(void)
  {
    pid_t p;
    int pipe_stdin[2], pipe_stdout[2];
    char execfile[1000] ;
    ssize_t size=readlink("/proc/self/exe",execfile,1000) ;
    execfile[size]='\0' ;
    std::string cmdline = "addr2line -e ";
    cmdline=cmdline+execfile ;
    
    int err ;
    err=pipe(pipe_stdin) ;
    // if (err) return -1; // make an exception//
    err=pipe(pipe_stdout) ;
     // if (err) return -1; // make an exception//

    p = fork();

    if(p < 0) return ; // make an exception//
    if(p == 0) 
    { /* child */
        close(pipe_stdin[1]);
        dup2(pipe_stdin[0], 0);
        close(pipe_stdout[0]);
        dup2(pipe_stdout[1], 1);
        execl("/bin/sh", "sh", "-c", cmdline.c_str(), NULL);
        std::cout<<"child exit !!!"<<std::endl ;
        perror("execl"); exit(99);
    }
    child_pid = p;
    to_child = pipe_stdin[1];
    from_child = pipe_stdout[0];
    close(pipe_stdin[0]);
    close(pipe_stdout[1]);
  }
  
  void CAddr2line::write(const std::string& str)
  {
    std::string strTmp=str+'\n' ;
    ::write(to_child, strTmp.data(), strTmp.size()) ;
  }

  void CAddr2line::read(std::string& str)
  {
    const int maxSize=10000 ;
    char buffer[maxSize] ;
    
    int a ;
    for(int i=0;i<maxSize;i++)
    {
      a = ::read(from_child,&buffer[i],1) ;
      if (a==-1) std::cout<<::strerror(errno)<<std::endl ;
      if(buffer[i]=='\n')
      {
        buffer[i]='\0' ;
        break ;
      }
    } 
    str=buffer ;
  }
  
  CAddr2line::~CAddr2line()
  {

    ::close(from_child) ;
    ::close(to_child) ;
    waitpid(child_pid, NULL, 0) ;
  }

}

#endif

#endif