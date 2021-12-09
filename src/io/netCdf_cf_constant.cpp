#include "netCdf_cf_constant.hpp"

namespace xios
{
  std::set<StdString> CCFConvention::XIOS_CF_Latitude_units_;
  std::set<StdString> CCFConvention::XIOS_CF_Longitude_units_;
  const StdString CCFKeywords::XIOS_CF_units("units");
  const StdString CCFKeywords::XIOS_CF_standard_name("standard_name");
  const StdString CCFKeywords::XIOS_CF_coordinates("coordinates");
  const StdString CCFKeywords::XIOS_CF_bounds("bounds");
}