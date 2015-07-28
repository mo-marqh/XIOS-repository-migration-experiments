#include "object_template_impl.hpp"
#include "xios_spl.hpp"
#include "node_type.hpp"

namespace xios
{
  template class CObjectTemplate<CContext>;
  template class CObjectTemplate<CCalendarWrapper>;
  template class CObjectTemplate<CField>;
  template class CObjectTemplate<CFile>;
  template class CObjectTemplate<CDomain>;
  template class CObjectTemplate<CGrid>;
  template class CObjectTemplate<CAxis>;
  template class CObjectTemplate<CVariable>;
  template class CObjectTemplate<CInverseAxis>;
  template class CObjectTemplate<CZoomAxis>;
  template class CObjectTemplate<CInterpolateAxis>;
  template class CObjectTemplate<CZoomDomain>;
  template class CObjectTemplate<CInterpolateFromFileDomain>;

  template class CObjectTemplate<CContextGroup>;
  template class CObjectTemplate<CFieldGroup>;
  template class CObjectTemplate<CFileGroup>;
  template class CObjectTemplate<CDomainGroup>;
  template class CObjectTemplate<CGridGroup>;
  template class CObjectTemplate<CAxisGroup>;
  template class CObjectTemplate<CVariableGroup>;
  template class CObjectTemplate<CInverseAxisGroup>;
  template class CObjectTemplate<CZoomAxisGroup>;
  template class CObjectTemplate<CInterpolateAxisGroup>;
  template class CObjectTemplate<CZoomDomainGroup>;
  template class CObjectTemplate<CInterpolateFromFileDomainGroup>;
}
