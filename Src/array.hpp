#ifndef __XMLIO_CArray__
#define __XMLIO_CArray__

/// boost headers ///
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/multi_array.hpp>

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"

namespace xmlioserver
{
   /// ////////////////////// Déclarations ////////////////////// ///
   template <typename ValueType, StdSize  NumDims,
             typename Allocator = std::allocator<ValueType> >
      class CArray
         : public boost::multi_array<ValueType, NumDims, Allocator>
   {
         /// Définition de type ///
         typedef boost::multi_array<ValueType, NumDims, Allocator> SuperClass;

      public:

         typedef ValueType ValType;

         /// Constructeurs ///
         template <typename ExtentList>
            explicit CArray(const ExtentList & sizes);

         template <typename ExtentList>
            CArray(const ExtentList & sizes, const boost::general_storage_order<NumDims> & store);

         CArray(const CArray & array);       // NEVER IMPLEMENTED.
         CArray(const CArray * const array); // NEVER IMPLEMENTED.

      public:
      
         /// Flux ///
         template <typename U, StdSize V, typename W>
            friend StdOStream & operator << 
                  (StdOStream & os, const CArray<U, V, W> & array);

         template <typename U, StdSize V, typename W>
            friend StdIStream & operator >> 
                  (StdIStream & is, CArray<U, V, W> & array);

      public:

         void toBinary  (StdOStream & os) const;
         void fromBinary(StdIStream & is);

         /// Destructeur ///
         virtual ~CArray(void);

   }; // class CArray

   ///---------------------------------------------------------------

} // namespace xmlioserver

#include "array_impl.hpp"
#include "array_mac.hpp"

#endif // __XMLIO_CArray__
