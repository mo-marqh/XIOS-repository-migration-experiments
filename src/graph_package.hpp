#ifndef __XIOS_CGraph_package__
#define __XIOS_CGraph_package__

namespace xios
{
  class CField;
  struct CGraphPackage
  {
    int filterId;
    std::vector< int > sourceFilterIds;
    std::vector< CField* > inFields;
    StdString contextId;
    bool show;
    std::pair< Time, Time > graphInterval;

    CGraphPackage(): show(true) {}
  };
  struct CGraphDataPackage
  {
    int fromFilter;
    int toFilter;
    StdString current_filter_name;
    CField *currentField=nullptr;
    int distanceFromStart=-1;
    StdString contextId;
    bool show;

    CGraphDataPackage(): fromFilter(-1), show(true) {}
  }; 
} // namespace xios

#endif //__XIOS_CGraph_package__
