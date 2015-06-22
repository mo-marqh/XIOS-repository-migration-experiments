#ifndef __XMLIO_TRANSFORMATION_ENUM__
#define __XMLIO_TRANSFORMATION_ENUM__

//#define DECLARE_NODE(Name_, name_)     ,e##Name_, g##Name_
//#define DECLARE_NODE_PAR(Name_, name_) ,e##Name_, g##Name_

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      typedef enum transformation_type
      {
        TRANS_ZOOM_AXIS,
        TRANS_INVERSE_AXIS
      } ETranformationType;

} // namespace xios

#endif // __XMLIO_TRANSFORMATION_ENUM__
