#ifndef __XMLIO_CMaximum__
#define __XMLIO_CMaximum__

/// xmlioserver headers ///
#include "functor.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CMaximum : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;
         typedef ARRAY(double, 1) DoubleArray;

         public :

            /// Constructeurs ///
            //CMaximum(void);                             // Not implemented.
            //CMaximum(const CFunData & data);
            CMaximum(DoubleArray doutput);
            //CMaximum(const CMaximum & Maximum);         // Not implemented.
            //CMaximum(const CMaximum * const Maximum);   // Not implemented.

            /// Traitement ///
            virtual void apply(const DoubleArray dinput, DoubleArray doutput);

            /// Destructeur ///
            virtual ~CMaximum(void);

      }; // class CMaximum

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CMaximum__
