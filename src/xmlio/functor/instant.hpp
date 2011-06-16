#ifndef __XMLIO_CInstant__
#define __XMLIO_CInstant__

/// xmlioserver headers ///
#include "functor.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CInstant : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;
         typedef ARRAY(double, 1) DoubleArray;

         public :

            /// Constructeurs ///
            //CInstant(void);                             // Not implemented.
            //CInstant(const CFunData & data);
            CInstant(DoubleArray doutput);
            //CInstant(const CInstant & instant);         // Not implemented.
            //CInstant(const CInstant * const instant);   // Not implemented.

            /// Traitement ///
            virtual void apply(const DoubleArray dinput, DoubleArray doutput);

            /// Destructeur ///
            virtual ~CInstant(void);

      }; // class CInstant

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CInstant__
