#ifndef __XMLIO_COnce__
#define __XMLIO_COnce__

/// xios headers ///
#include "functor.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class COnce : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;
         typedef ARRAY(double, 1) DoubleArray;

         public :

            /// Constructeurs ///
            //COnce(void);                       // Not implemented.
            COnce(DoubleArray doutput);
            //COnce(const COnce & once);         // Not implemented.
            //COnce(const COnce * const once);   // Not implemented.

            /// Traitement ///
            virtual void apply(const DoubleArray dinput, DoubleArray doutput);

            /// Destructeur ///
            virtual ~COnce(void);

      }; // class COnce

   } // namespace func
} // namespace xios

#endif //__XMLIO_COnce__

