#include "average.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CAverage::CAverage(DoubleArray doutput)
         : SuperClass(StdString("average"), doutput)
      { /* Ne rien faire de plus */ }

      CAverage::~CAverage(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CAverage::apply(const DoubleArray         UNUSED(dinput),
                                 DoubleArray         UNUSED(doutput))
      {
         ERROR("CAverage::apply(...)", << "Not implemented yet !");
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
