#include "instant.hpp"

namespace xios
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

      void CInstant::apply(const DoubleArray _dinput,
                                 DoubleArray _doutput)
      {
       	 const double * it1  = _dinput->data(),
       	              * end1 = _dinput->data() + _dinput->num_elements();
       	 double * it   = _doutput->data();
       	 for (; it1 != end1; it1++, it++) *it  = *it1;
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
