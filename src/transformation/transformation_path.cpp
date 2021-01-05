#include "transformation_path.hpp"

namespace xios
{
  void CTransformationPaths::mergePaths(const CTransformationPaths& transformationPaths)
  {
    donePath_ = transformationPaths.donePath_ ;
    if (donePath_.empty()) donePath_.push_back(transformationPaths.path_) ; // entry point
    
    if (get<1>(remainPath_)!="") return ;

    if (!donePath_.empty())
    {
      TPath& remotePath = donePath_.back() ;
      
      // same element type and element id
      if (std::get<0>(remotePath)==std::get<0>(path_) && std::get<1>(remotePath)==std::get<1>(path_))
      {
        auto it = get<2>(path_).begin() ;
        auto remoteIt = get<2>(remotePath).begin() ;
        for(  ; remoteIt !=get<2>(remotePath).end() ; remoteIt++ )
        {
          if (it==get<2>(path_).end() || *it != *remoteIt) break ;
          else it++ ;
        }

        if (remoteIt==get<2>(remotePath).end()) 
        {
          get<0>(remainPath_) = std::get<0>(path_) ;
          get<1>(remainPath_) = std::get<1>(path_) ;
          get<2>(remainPath_).insert( get<2>(remainPath_).begin(), it , std::get<2>(path_).end() ) ;
        }
        else  remainPath_ = path_ ;

      }
      else remainPath_=path_ ;
    }
    else
    {
      remainPath_=path_ ;
    } 
    get<1>(path_).clear() ;
    get<2>(path_).clear() ;
  }
  
  void CTransformationPaths::mergePaths(void)
  {
    CTransformationPaths transformationPath ;
   /* TPath newPath = path_ ;
    get<2>(newPath).clear() ;
    transformationPath.donePath_.push_back(newPath) ; */
    mergePaths(transformationPath) ;
  }

  CTransformationPaths CTransformationPaths::getDonePath(void)
  {
    CTransformationPaths returnDonePath ;
    returnDonePath.donePath_ = donePath_ ;
    return  returnDonePath ;
  }

  EElement CTransformationPaths::getNextElementType(void)
  {
    if (get<1>(remainPath_)=="") return get<0>(donePath_.back());
    else return get<0>(remainPath_) ;
  }

  string CTransformationPaths::getNextElementId(void)
  {
    CTransformationPaths transformationPath = *this ;
    transformationPath.removeNextTransform() ;
    return transformationPath.getPathsId(transformationPath.donePath_) ;
    /*
    string sep="/" ;
    string doneId=getPathsId(donePath_) ;
    if (doneId=="") sep="" ;
    TPath next = remainPath_ ;
    get<2>(next).erase(++(get<2>(next).begin()),get<2>(next).end()) ;
    string remainId=getPathId(next) ;
    if (remainId=="") sep="" ;
    return doneId+sep+remainId ;
   */
  }

  string CTransformationPaths::getNextElementSrcId(void)
  {
    return getPathsId(donePath_) ;
  }

  ETranformationType CTransformationPaths::getNextTransformationType(void)
  {
    return get<2>(remainPath_).front().first ;
  }

  string CTransformationPaths::getNextTransformationId(void)
  {
    return get<2>(remainPath_).front().second ;
  }

  void CTransformationPaths::removeNextTransform(void)
  {
    if (get<1>(remainPath_)!="")
    {
      TPath newPath;
      get<0>(newPath)=get<0>(remainPath_) ;
      get<1>(newPath)=get<1>(remainPath_) ;
      if (!get<2>(remainPath_).empty()) get<2>(newPath).push_back(get<2>(remainPath_).front()) ;
      if (donePath_.empty()) donePath_.push_back(newPath);
      else
      {
        if (get<0>(donePath_.back())==get<0>(newPath) && get<1>(donePath_.back())==get<1>(newPath)) 
          get<2>(donePath_.back()).push_back(get<2>(newPath).front()) ;
        else donePath_.push_back(newPath);
      }
      if (!get<2>(newPath).empty()) get<2>(remainPath_).pop_front() ;
      if (get<2>(remainPath_).empty()) get<1>(remainPath_)="" ;
    }
    else
    {
      if (!get<2>(remainPath_).empty())
      {
        get<2>(donePath_.back()).push_back(get<2>(remainPath_).front()) ;
        get<2>(remainPath_).pop_front() ;
      }
    }
  }


  string CTransformationPaths::getPathId(const TPath& path)
  {
    string id,id1 ;
    if (get<1>(path)!="")
    {
      if (get<0>(path)==EElement::DOMAIN) id="CDomain" ;
      if (get<0>(path)==EElement::AXIS)   id="CAxis" ;
      if (get<0>(path)==EElement::SCALAR) id="CScalar" ;
      id=id+":"+get<1>(path) ;
      id1="/" ;
    }

    for(auto transform : get<2>(path))
    {
      
      id=id+id1 ;
      id=id+to_string(transform.first) ; // not very nice but enough for now. Should be replace by string tranformation name
      id=id+":"+transform.second ;
      id1="/" ;
    }
    return id ;
  }

  string CTransformationPaths::getPathsId(const list<TPath>& paths)
  {
    string id ;
    string sep("") ;
    for(auto path:paths) 
    {
      id=id+sep+getPathId(path) ;
      sep="//" ;
    }
    return id ;
  }
}