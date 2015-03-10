/*!
   \file utils.hpp
   \author Ha NGUYEN
   \since 06 Oct 2014
   \date 10 Feb 2015


   \brief Some utils for Xios
 */

#ifndef __XIOS_UTILS_HPP__
#define __XIOS_UTILS_HPP__

#include <vector>
#include "array_new.hpp"
#include "exception.hpp"

namespace xios
{
  template<typename K>
  struct CArrayTraits {
    typedef K ArrayType;
  };

  template<typename K>
  struct CArrayBoolTraits : public CArrayTraits<K> {
    typedef bool Type;
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,1> >
  {
    static inline void resizeArray(CArray<bool,1>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (1 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 1 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0]);
    }
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,2> >
  {
    static inline void resizeArray(CArray<bool,2>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (2 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 2 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0], dimensionSize[1]);
    }
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,3> >
  {
    static inline void resizeArray(CArray<bool,3>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (3 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 3 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0], dimensionSize[1], dimensionSize[2]);
    }
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,4> >
  {
    static inline void resizeArray(CArray<bool,4>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (4 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 4 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0], dimensionSize[1], dimensionSize[2], dimensionSize[3]);
    }
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,5> >
  {
    static inline void resizeArray(CArray<bool,5>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (5 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 5 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0], dimensionSize[1],
                       dimensionSize[2], dimensionSize[3], dimensionSize[4]);
    }
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,6> >
  {
    static inline void resizeArray(CArray<bool,6>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (6 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 6 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0], dimensionSize[1],
                       dimensionSize[2], dimensionSize[3],
                       dimensionSize[4], dimensionSize[5]);
    }
  };

  template<>
  struct CArrayBoolTraits<CArray<bool,7> >
  {
    static inline void resizeArray(CArray<bool,7>& boolArray, const std::vector<int>& dimensionSize)
    {
      if (7 != dimensionSize.size())
        ERROR("utils::CArrayBoolTraits",
                <<"Dimension of resized array mismatch"<<endl
                <<"Dimension of resized is 7 "<<endl
                <<"Dimension of vetor resizing is "<< dimensionSize.size());
      boolArray.resize(dimensionSize[0], dimensionSize[1],
                       dimensionSize[2], dimensionSize[3],
                       dimensionSize[4], dimensionSize[5], dimensionSize[6]);
    }
  };

}

#endif // __UTILS_HPP__
