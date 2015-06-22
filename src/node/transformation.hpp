#ifndef __XIOS_CTransformation__
#define __XIOS_CTransformation__

#include "xios_spl.hpp"
#include "transformation_enum.hpp"

namespace xios {

  ///--------------------------------------------------------------
  /*!
    \class CTransformation
    This class describes inverse_axis in xml file.
  */
  template<typename T>
  class CTransformation
  {
  public:
    typedef typename std::list<std::pair<ETranformationType, CTransformation<T>* > > TransformationMapTypes;
    typedef TransformationMapTypes TransMapTypes;

    public :
      /// Constructeurs ///
      CTransformation(void) {}
      virtual void checkValid(T* dest) = 0;

      /// Destructeur ///
      virtual ~CTransformation(void) {}
  }; // class CTransformation

} // namespace xios

#endif // __XIOS_CTransformation__
