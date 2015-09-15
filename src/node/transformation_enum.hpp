#ifndef __XIOS_TRANSFORMATION_ENUM__
#define __XIOS_TRANSFORMATION_ENUM__

namespace xios
{
      /// ////////////////////// Définitions ////////////////////// ///
      typedef enum transformation_type
      {
        TRANS_ZOOM_AXIS,
        TRANS_INVERSE_AXIS,
        TRANS_INTERPOLATE_AXIS,
        TRANS_ZOOM_DOMAIN,
        TRANS_INTERPOLATE_DOMAIN,
        TRANS_GENERATE_RECTILINEAR_DOMAIN
      } ETranformationType;

} // namespace xios

#endif // __XIOS_TRANSFORMATION_ENUM__
