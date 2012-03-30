#include "minimum.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CMinimum::CMinimum(DoubleArray doutput)
         : SuperClass(StdString("minimum"), doutput)
      { /* Ne rien faire de plus */ }

      CMinimum::~CMinimum(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CMinimum::apply(const DoubleArray _dinput,
                                 DoubleArray _doutput)
      {
         const double * it1  = _dinput->data(),
                      * end1 = _dinput->data() + _dinput->num_elements();
         double * it   = _doutput->data();
         if (this->nbcall == 1)
              for (; it1 != end1; it1++, it++) *it = *it1;
         else for (; it1 != end1; it1++, it++) *it = std::min(*it1, *it);
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
