#ifndef __XMLIO_CAverage__
#define __XMLIO_CAverage__

/// xmlioserver headers ///
#include "functor.hpp"

namespace xios
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CAverage : public CFunctor
      {
         /// Définition de type ///
         typedef CFunctor SuperClass;
         typedef ARRAY(double, 1) DoubleArray;

         public :

            /// Constructeurs ///
            //CAverage(void);                             // Not implemented.
            //CAverage(const CFunData & data);
            CAverage(DoubleArray doutput);
            //CAverage(const CAverage & average);         // Not implemented.
            //CAverage(const CAverage * const average);   // Not implemented.

            /// Traitement ///
            virtual void apply(const DoubleArray dinput, DoubleArray doutput);
            virtual void final(void) ;
            
            /// Destructeur ///
            virtual ~CAverage(void);

      }; // class CAverage

   } // namespace func
} // namespace xmlioserver

#endif //__XMLIO_CAverage__
