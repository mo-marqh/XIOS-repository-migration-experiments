#include "maximum.hpp"
#include <algorithm>


namespace xios
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CMaximum::CMaximum(DoubleArray doutput)
         : SuperClass(StdString("maximum"), doutput)
      { /* Ne rien faire de plus */ }

      CMaximum::~CMaximum(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      void CMaximum::apply(const DoubleArray _dinput,
                                 DoubleArray _doutput)
      {
       	 const double * it1  = _dinput->data(),
       	              * end1 = _dinput->data() + _dinput->num_elements();
       	       double * it   = _doutput->data();
         if (this->nbcall == 1)
              for (; it1 != end1; it1++, it++) *it = *it1;
         else for (; it1 != end1; it1++, it++) *it = std::max(*it1, *it);

         it1  = _dinput->data(),
         end1 = _dinput->data() + _dinput->num_elements();
       	 it  = _doutput->data();
         double sum=0 ;
         for (; it1 != end1; it1++, it++) sum+=*it;
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
