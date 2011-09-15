/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, XMLIOServer, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */


#ifndef __ICUTIL_HPP__
#define __ICUTIL_HPP__

#include <string>

// ///////////////////////// Définitions/Déclarations /////////////////////// //

inline bool cstr2string(const char * cstr, int cstr_size, std::string & str)
{
  std::string valtemp;
  std::size_t d, f = 0;
  if (cstr_size != -1) 
  { 
     valtemp.append (cstr, cstr_size);
     d = valtemp.find_first_not_of(' ');
     f = valtemp.find_last_not_of (' ');
     str = valtemp.substr(d, f-d+1); 
     return (true);
  }
  else
  {
     return (false);
  }  
}

#endif // __ICUTIL_HPP__
