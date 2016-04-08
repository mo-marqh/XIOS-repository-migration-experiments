#ifndef __XIOS_TRANSFORMATION_ENUM__
#define __XIOS_TRANSFORMATION_ENUM__

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      typedef enum transformation_type
      {
        TRANS_ZOOM_AXIS = 0,
        TRANS_INVERSE_AXIS = 1,
        TRANS_INTERPOLATE_AXIS = 2,
        TRANS_ZOOM_DOMAIN = 3,
        TRANS_INTERPOLATE_DOMAIN = 4,
        TRANS_GENERATE_RECTILINEAR_DOMAIN = 5
      } ETranformationType;

} // namespace xios

#endif // __XIOS_TRANSFORMATION_ENUM__
