#include "field_impl.hpp"

namespace xios
{
  template void CField::setData<1>(const ARRAY(double, 1) _data) ;
  template void CField::setData<2>(const ARRAY(double, 2) _data) ;
  template void CField::setData<3>(const ARRAY(double, 3) _data) ;

}
