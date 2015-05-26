#include "field_impl.hpp"

namespace xios
{
  template void CField::setData<1>(const CArray<double, 1>& _data);
  template void CField::setData<2>(const CArray<double, 2>& _data);
  template void CField::setData<3>(const CArray<double, 3>& _data);

  template void CField::getData<1>(CArray<double, 1>& _data) const;
  template void CField::getData<2>(CArray<double, 2>& _data) const;
  template void CField::getData<3>(CArray<double, 3>& _data) const;
}
