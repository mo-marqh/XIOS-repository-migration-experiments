#ifndef __XIOS_INETCDF4_IMPL__
#define __XIOS_INETCDF4_IMPL__

#include "inetcdf4.hpp"
#include "netCdfInterface.hpp"

namespace xios
{
//const StdString CFLatUnits[] = {"degrees_north", "degree_north", "degree_N", "degrees_N", "degreeN", "degreesN"};
//const StdString CFLonUnits[] = {"degrees_east", "degree_east", "degree_E", "degrees_E", "degreeE", "degreesE"};

struct CCFKeywords
{
  static const StdString XIOS_CF_units;
  static const StdString XIOS_CF_standard_name;
  static const StdString XIOS_CF_coordinates;
  static const StdString XIOS_CF_bounds;
};

struct CCFConvention
{
  static const std::set<StdString>& XIOS_CF_Latitude_units()
  {
    if (XIOS_CF_Longitude_units_.empty()) XIOS_CF_Latitude_units_= {"degrees_north", "degree_north", "degree_N", "degrees_N", "degreeN", "degreesN"};
    return XIOS_CF_Latitude_units_ ;
  }

  static const std::set<StdString>& XIOS_CF_Longitude_units()
  {
    if (XIOS_CF_Longitude_units_.empty()) XIOS_CF_Longitude_units_= {"degrees_east", "degree_east", "degree_E", "degrees_E", "degreeE", "degreesE"};
    return XIOS_CF_Longitude_units_ ;
  }
  static void releaseStaticAllocation(void) { XIOS_CF_Latitude_units_.clear() ; XIOS_CF_Longitude_units_.clear() ; }
private:
  CCFConvention();
  static std::set<StdString> XIOS_CF_Latitude_units_;
  static std::set<StdString> XIOS_CF_Longitude_units_;
};

} // namespace xios

#endif //__XIOS_INETCDF4_IMPL__
