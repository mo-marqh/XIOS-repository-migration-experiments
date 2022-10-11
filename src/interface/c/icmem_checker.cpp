
#include "mem_checker.hpp"
#include <string>
#include <iostream>


using namespace xios;

extern "C"
{
  void cxios_mem_checker_get(double* mem)
  {
    *mem=CMemChecker::getMemRSS();
  }

  void cxios_mem_checker_log(const char* mem_id, int len_mem_id, bool* finalize)
  {
    std::string str(mem_id,len_mem_id);
    CMemChecker::logMem(str, *finalize);
  }

}


