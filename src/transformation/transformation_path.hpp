#ifndef __XIOS_TRANSFORMATION_PATH_HPP__
#define __XIOS_TRANSFORMATION_PATH_HPP__

#include "transformation_enum.hpp"
#include "element_type.hpp"
#include "xios_spl.hpp"

namespace xios
{
  class CTransformationPaths 
  {
    public:
      typedef std::tuple<EElement, string, std::list<std::pair<ETranformationType,std::string>>> TPath ;
    
    private:
      TPath path_ ;
      std::list<TPath> donePath_ ; 
      TPath remainPath_ ; 
      string getPathId(const TPath& path) ;
      string getPathsId(const list<TPath>& paths) ;
    
    public:
      void addPath(TPath& path) {path_=path;}
      void mergePaths(const CTransformationPaths& transformationPath) ;
      void mergePaths(void) ;
      bool hasTransform(void) { return !std::get<2>(remainPath_).empty();}
      bool hasPath(void) { return !std::get<1>(path_).empty() ;}
      CTransformationPaths getDonePath(void) ;

      EElement getNextElementType(void);
      std::string getNextElementId(void) ;
      std::string getNextElementSrcId(void) ;
      ETranformationType getNextTransformationType(void) ;
      std::string getNextTransformationId(void) ;
      void removeNextTransform(void) ;
  };

}

#endif // __XIOS_TRANSFORMATION_PATH_HPP__