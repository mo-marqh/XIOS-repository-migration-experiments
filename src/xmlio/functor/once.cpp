#include "once.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      COnce::COnce(DoubleArray doutput)
         : SuperClass(StdString("once"), doutput)
      { /* Ne rien faire de plus */ }

      COnce::~COnce(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void COnce::apply(const DoubleArray         UNUSED(dinput),
                              DoubleArray         UNUSED(doutput))
      {
         ERROR("COnce::apply(...)", << "Not implemented yet !");
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver

