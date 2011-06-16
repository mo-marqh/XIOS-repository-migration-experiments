#include "instant.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CInstant::CInstant(DoubleArray doutput)
         : SuperClass(StdString("instant"), doutput)
      { /* Ne rien faire de plus */ }

      CInstant::~CInstant(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CInstant::apply(const DoubleArray         UNUSED(dinput),
                                 DoubleArray         UNUSED(doutput))
      {
         ERROR("CInstant::apply(...)", << "Not implemented yet !");
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
