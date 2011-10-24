#ifndef __XMLIO_CFunctor__
#define __XMLIO_CFunctor__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "array.hpp"

namespace xmlioserver
{
   namespace func
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CFunctor : public CObject
      {
         /// Définition de type ///
         typedef CObject SuperClass;
         typedef ARRAY(double, 1) DoubleArray;

         public :

            /// Accesseurs ///
            DoubleArray getDataOutput(void) const;
            /// Opérateur ///
            DoubleArray operator ()(const DoubleArray dinput);

            /// Destructeur ///
            virtual ~CFunctor(void);

            //Traitement ///
            virtual void final(void);

         protected :

            /// Traitement ///
            virtual void apply(const DoubleArray dinput, DoubleArray doutput) = 0;

            /// Autres ///
            virtual StdString toString(void) const;
            virtual void fromString(const StdString & str);

            /// Constructeurs ///
            CFunctor(void);                             // Not implemented.
            CFunctor(const StdString & id, DoubleArray doutput);
            CFunctor(const CFunctor & functor);         // Not implemented.
            CFunctor(const CFunctor * const functor);   // Not implemented.

         private :

            /// Propriétés privées ///
            DoubleArray doutput;
            
         protected :
            /// Propriétés protégées ///   
            int nbcall;            
      }; // class CFunctor
   } // namespace func
} // namespace xmlioserver

#include "functor_type.hpp"

#endif // __XMLIO_CFunctor__
