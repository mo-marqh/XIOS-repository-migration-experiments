#include "release_static_allocation.hpp"
#include "operator_expr.hpp"
#include "netCdf_cf_constant.hpp"
#include "context.hpp"
#include "server_context.hpp"
#include "timer.hpp"
#include "mem_checker.hpp"

namespace xios
{
  void releaseStaticAllocation(void)
  {
    CContext::releaseStaticAllocation() ; // free memory from static allocation
    CCFConvention::releaseStaticAllocation() ; // free memory from static allocation
    CServerContext::releaseStaticAllocation() ;
    CTimer::release() ;
    CMemChecker::release() ;
    operatorExpr.release();
  }  
}