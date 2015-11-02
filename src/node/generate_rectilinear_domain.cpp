#include "generate_rectilinear_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CGenerateRectilinearDomain::CGenerateRectilinearDomain(void)
    : CObjectTemplate<CGenerateRectilinearDomain>(), CGenerateRectilinearDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CGenerateRectilinearDomain::CGenerateRectilinearDomain(const StdString & id)
    : CObjectTemplate<CGenerateRectilinearDomain>(id), CGenerateRectilinearDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CGenerateRectilinearDomain::~CGenerateRectilinearDomain(void)
  {}

  //----------------------------------------------------------------

  StdString CGenerateRectilinearDomain::GetName(void)    { return StdString("generate_rectilinear_domain"); }
  StdString CGenerateRectilinearDomain::GetDefName(void) { return StdString("generate_rectilinear_domain"); }
  ENodeType CGenerateRectilinearDomain::GetType(void)    { return eGenerateRectilinearDomain; }

  void CGenerateRectilinearDomain::checkValid(CDomain* domainDst)
  {
    const double defaultBndsLonStart = 0;
    const double defaultBndsLonEnd = 360;

    const double defaultBndsLatStart = -90;
    const double defaultBndsLatEnd = 90;

    if ((!lon_start.isEmpty() && lon_end.isEmpty()) ||
        (lon_start.isEmpty() && !lon_end.isEmpty()))
         ERROR("CGenerateRectilinearDomain::checkValid(CDomain* domainDst)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only longitude start or longitude end attribute is defined." << std::endl
               << "Must define both: 'lon_start' and 'lon_end'.");

    if ((!lat_start.isEmpty() && lat_end.isEmpty()) ||
        (lat_start.isEmpty() && !lat_end.isEmpty()))
         ERROR("CGenerateRectilinearDomain::checkValid(CDomain* domainDst)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only latitude start or latitude end attribute is defined." << std::endl
               << "Must define both: 'lat_start' and 'lat_end'.");

    if ((!bounds_lon_start.isEmpty() && bounds_lon_end.isEmpty()) ||
        (bounds_lon_start.isEmpty() && !bounds_lon_end.isEmpty()))
         ERROR("CGenerateRectilinearDomain::checkValid(CDomain* domainDst)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only longitude boundary start or longitude boundary end attribute is defined." << std::endl
               << "Must define both: 'bounds_lon_start' and 'bounds_lon_end'.");

    if ((!bounds_lat_start.isEmpty() && bounds_lat_end.isEmpty()) ||
        (bounds_lat_start.isEmpty() && !bounds_lat_end.isEmpty()))
         ERROR("CGenerateRectilinearDomain::checkValid(CDomain* domainDst)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only latitude boundary start or latitude boundary end attribute is defined." << std::endl
               << "Must define both: 'bounds_lat_start' and 'bounds_lat_end'.");

    if (!bounds_lon_start.isEmpty() && !lon_start.isEmpty())
         ERROR("CGenerateRectilinearDomain::checkValid(CDomain* domainDst)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one longitude boundary attribute or longitude can be used but both 'bounds_lon_start' and 'lon_start' are defined." << std::endl
               << "Define only one attribute: 'bounds_lon_start' or 'lon_start'.");

    if (!bounds_lat_start.isEmpty() && !lat_start.isEmpty())
         ERROR("CGenerateRectilinearDomain::checkValid(CDomain* domainDst)",
               << "[ id = " << this->getId() << " , context = '" << CObjectFactory::GetCurrentContextId() << " ] "
               << "Only one latitude boundary attribute or latitude can be used but both 'bounds_lat_start' and 'lat_start' are defined." << std::endl
               << "Define only one attribute: 'bounds_lat_start' or 'lat_start'.");

    if (bounds_lon_start.isEmpty() && lon_start.isEmpty())
    {
      bounds_lon_start.setValue(defaultBndsLonStart);
      bounds_lon_end.setValue(defaultBndsLonEnd);
    }

    if (bounds_lat_start.isEmpty() && lat_start.isEmpty())
    {
      bounds_lat_start.setValue(defaultBndsLatStart);
      bounds_lat_end.setValue(defaultBndsLatEnd);
    }

    if (!bounds_lon_start.isEmpty())
    {
      int niGlo = domainDst->ni_glo.getValue();

      double boundsLonRange = bounds_lon_end - bounds_lon_start;
      double boundsLonStep = boundsLonRange/(double(niGlo));
      domainDst->bounds_lon_start.setValue(bounds_lon_start);
      domainDst->bounds_lon_end.setValue(bounds_lon_end);
      domainDst->lon_start.setValue(bounds_lon_start + boundsLonStep/2);
      domainDst->lon_end.setValue( bounds_lon_end   - boundsLonStep/2);
    }

    if (!bounds_lat_start.isEmpty())
    {
      int njGlo = domainDst->nj_glo.getValue();

      double boundsLatRange = bounds_lat_end - bounds_lat_start;
      double boundsLatStep = boundsLatRange/(double(njGlo));
      domainDst->bounds_lat_start.setValue(bounds_lat_start);
      domainDst->bounds_lat_end.setValue(bounds_lat_end);
      domainDst->lat_start.setValue(bounds_lat_start + boundsLatStep/2);
      domainDst->lat_end.setValue(bounds_lat_end   - boundsLatStep/2);
    }

    if (lon_start.isEmpty() && lat_start.isEmpty()) return;

    if (!lon_start.isEmpty())
    {
      int niGlo = domainDst->ni_glo.getValue();

      double lonRange = lon_end - lon_start;
      double lonStep = (1 == niGlo) ? lonRange : lonRange/(double(niGlo)-1);
      domainDst->lon_start.setValue(lon_start);
      domainDst->lon_end.setValue(lon_end);
      domainDst->bounds_lon_start.setValue(lon_start - lonStep/2);
      domainDst->bounds_lon_end.setValue(lon_end   + lonStep/2);
    }

    if (!lat_start.isEmpty())
    {
      int njGlo = domainDst->nj_glo.getValue();

      double latRange = lat_end - lat_start;
      double latStep = (1 == njGlo) ? latRange : latRange/(double(njGlo)-1);
      domainDst->lat_start.setValue(lat_start);
      domainDst->lat_end.setValue(lat_end);
      domainDst->bounds_lat_start.setValue(lat_start - latStep/2);
      domainDst->bounds_lat_end.setValue(lat_end   + latStep/2);
    }
  }

}
