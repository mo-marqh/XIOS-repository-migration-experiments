#ifndef __XMLIO_CArray__
#define __XMLIO_CArray__

/// boost headers ///
#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/multi_array.hpp>

/// xios headers ///
#include "xmlioserver_spl.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"


namespace xios
{
   template<size_t numDims>
   detail::multi_array::extent_gen<numDims> getExtentNull(void) { return getExtentNull<numDims-1>()[0];}
   
   template<>
   detail::multi_array::extent_gen<1> getExtentNull<1>(void) { return extents[0]; }

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

         explicit CArray();

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

         size_t getSize(void) const ;
         bool toBuffer  (CBufferOut& buffer) const;
         bool fromBuffer(CBufferIn& buffer);


         /// Destructeur ///
         virtual ~CArray(void);

   }; // class CArray
   

   ///---------------------------------------------------------------

} // namespace xios

#include "array_impl.hpp"
#include "array_mac.hpp"

#endif // __XMLIO_CArray__
