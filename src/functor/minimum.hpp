#ifndef __XMLIO_CMinimum__
#define __XMLIO_CMinimum__

/// xmlioserver headers ///
#include "functor.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CMinimum : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;
         typedef ARRAY(double, 1) DoubleArray;

         public :

            /// Constructeurs ///
            //CMinimum(void);                             // Not implemented.
            //CMinimum(const CFunData & data);
            CMinimum(DoubleArray doutput);
            //CMinimum(const CMinimum & Minimum);         // Not implemented.
            //CMinimum(const CMinimum * const Minimum);   // Not implemented.

            /// Traitement ///
            virtual void apply(const DoubleArray dinput, DoubleArray doutput);

            /// Destructeur ///
            virtual ~CMinimum(void);

      }; // class CMinimum

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CMinimum__
