#ifndef __XMLIO_CTransformation__
#define __XMLIO_CTransformation__

#include "xmlioserver_spl.hpp"
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
    typedef typename boost::unordered_map<ETranformationType, CTransformation<T>*, boost::hash<int> > TransformationMapTypes;
    typedef TransformationMapTypes TransMapTypes;

    public :
      /// Constructeurs ///
      CTransformation(void) {}
      virtual void checkValid(T* dest) = 0;

      /// Destructeur ///
      virtual ~CTransformation(void) {}
  }; // class CTransformation

} // namespace xios

#endif // __XMLIO_CTransformation__
