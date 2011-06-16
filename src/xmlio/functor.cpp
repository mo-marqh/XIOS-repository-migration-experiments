#include "functor.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CFunctor::CFunctor(const StdString & id, ARRAY(double, 1) doutput)
         : SuperClass(id), doutput(doutput)
      { /* Ne rien faire de plus */  }

      CFunctor::~CFunctor(void)
      { /* Ne rien faire de plus */ }

      //---------------------------------------------------------------

      ARRAY(double, 1) CFunctor::getDataOutput(void) const
      { 
         return (this->doutput);
      }

      //---------------------------------------------------------------

      StdString CFunctor::toString(void) const
      {
         ERROR("CFunctor::toString()", << "Not implemented yet !");
         return (SuperClass::getId());
      }

      void CFunctor::fromString(const StdString & str)
      {
         ERROR("CFunctor::fromString(str)",
                << "[ str = " << str << "] Not implemented yet !");
      }

      //---------------------------------------------------------------

      ARRAY(double, 1) CFunctor::operator ()(const ARRAY(double, 1) dinput)
      {
         if (dinput->size() != this->doutput->size())
            ERROR("CFunctor::operator ()(dinput)",
                   << "[ input size = "  << dinput->size()
                   << ", output size = " << this->doutput->size() << " ]"
                   << " size of input array !=  size of output array !");
         this->apply(dinput, this->doutput);
         return (this->doutput);
      }

      //---------------------------------------------------------------

   } // namespace func
} // namespace xmlioserver
