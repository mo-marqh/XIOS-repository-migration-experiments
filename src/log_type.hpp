#ifndef __XIOS_LOG_TYPE_HPP__
#define __XIOS_LOG_TYPE_HPP__

#include <string>
#include <iostream>
#include <cstdint>
#include <unordered_map>

//#include "cxios.hpp"
#include "string_tools.hpp"

namespace xios
{
  class CSetLog ;

  class CLogType
  {
    public: 

    CLogType(void) {}
    CLogType(const std::string& name) ;

    static void bitSet(CLogType& log, int bitPos) { log.type_ |= (uint64_t)1 << bitPos ;}
    static bool bitAnd(CLogType& log1, CLogType& log2) { return (log1.type_ & log2.type_) == log2.type_ ; }
    
    private:
    
    uint64_t type_=0 ;
    int bitPos_=0 ;
    static int globalBitPos_ ;
    
  } ;

  class CSetLog
  {
    public:

    CSetLog(void) ;
    bool isSet(CLogType& log) { return CLogType::bitAnd(logType_, log) ; }

    private:
    
    static int registerLog(const std::string& name, int bitPos) ;
    static std::map<std::string, int>& getRegistredLog(void) ;
    
    CLogType logType_ ;
    friend class CLogType ;
  } ;



}

#endif