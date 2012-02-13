#ifndef __XMLIO_CArray_impl__
#define __XMLIO_CArray_impl__

#include "array_mac.hpp"
#include "buffer_in.hpp"
#include "buffer_out.hpp"

namespace xmlioserver
{
   
   /// ////////////////////// Définitions ////////////////////// ///

   template <typename ValueType, StdSize NumDims, typename Allocator>
      template <typename ExtentList>
         CArray<ValueType, NumDims, Allocator>::CArray(const ExtentList & sizes)
            : boost::multi_array<ValueType, NumDims, Allocator>
                  (sizes, boost::fortran_storage_order())
   { /* Ne rien faire de plus */ }

   template <typename ValueType, StdSize NumDims, typename Allocator>
       CArray<ValueType, NumDims, Allocator>::CArray()
            : boost::multi_array<ValueType, NumDims, Allocator>
                  (getExtentNull<NumDims>(), boost::fortran_storage_order())
   { /* Ne rien faire de plus */ }
   
   template <typename ValueType, StdSize NumDims, typename Allocator>
      template <typename ExtentList>
         CArray<ValueType, NumDims, Allocator>::CArray
            (const ExtentList & sizes, const boost::general_storage_order<NumDims> & store)
               : boost::multi_array<ValueType, NumDims, Allocator> (sizes, store)
   { /* Ne rien faire de plus */ }

   template <typename ValueType, StdSize NumDims, typename Allocator>
      CArray<ValueType, NumDims, Allocator>::~CArray(void)
   { /* Ne rien faire de plus */ }

   //----------------------------------------------------------------

   template <typename ValueType, StdSize NumDims, typename Allocator>
      StdOStream & operator << (StdOStream & os,
                                const CArray<ValueType, NumDims, Allocator> & array)
   {
      os << "CArray (" ;
      for (StdSize i = 1; i < array.num_dimensions(); i++)
         os << ", " << array.shape()[i];
      os << ") = " ; 
      for (StdSize i = 0; i < array.num_elements(); i++) os << (array.data()[i])<<"  ";
      return (os);
   }

   //----------------------------------------------------------------

   template <typename ValueType, StdSize NumDims, typename Allocator>
      StdOStream & operator << 
         (StdOStream & os, const boost::shared_ptr<CArray<ValueType, NumDims, Allocator> > & array)
   { 
      os << *array; 
      return (os); 
   }
   
   //----------------------------------------------------------------
   
   template <typename ValueType> void FromBinary
         (StdIStream & is, ARRAY(ValueType, 1) & array)
   {
      ARRAY_ASSIGN(array, ValueType, 1, [1]);
      array->fromBinary(is);
   }
   
   template <typename ValueType> void FromBinary
         (StdIStream & is, ARRAY(ValueType, 2) & array)
   {
      ARRAY_ASSIGN(array, ValueType, 2, [1][1]);
      array->fromBinary(is);
   }

   template <typename ValueType> void FromBinary
         (StdIStream & is, ARRAY(ValueType, 3) & array)
   {
      ARRAY_ASSIGN(array, ValueType, 3, [1][1][1]);
      array->fromBinary(is);
   }
   //----------------------------------------------------------------

   template <typename ValueType, StdSize NumDims, typename Allocator>
      void CArray<ValueType, NumDims, Allocator>::toBinary  (StdOStream & os) const
   {
      typedef boost::multi_array_types::size_type LSize;
      LSize nelem = this->num_elements();
      LSize ndim  = this->num_dimensions();
      const LSize * shape = this->shape();
      const ValueType * data = this->data();

      os.write (reinterpret_cast<const char*>(&ndim) , sizeof(LSize));
      for (LSize i = 0; i < ndim; i++ )
         os.write (reinterpret_cast<const char*>(&(shape[i])), sizeof(LSize));
      os.write (reinterpret_cast<const char*>(&nelem), sizeof(LSize));
      os.write (reinterpret_cast<const char*>(data), nelem * sizeof(ValueType));
   }

   //----------------------------------------------------------------

   template <typename ValueType, StdSize NumDims, typename Allocator>
      void CArray<ValueType, NumDims, Allocator>::fromBinary(StdIStream & is)
   {
      typedef boost::multi_array_types::size_type LSize;
      LSize ndim = 0, nelem = 0, temp = 0;
      std::vector<LSize> shape;
      is.read (reinterpret_cast<char*>(&ndim) , sizeof(LSize));
      for (LSize i = 0; i < ndim; i++ )
      {
         is.read (reinterpret_cast<char*>(&temp) , sizeof(LSize));
         shape.push_back(temp);
      }
      this->resize(shape);
      is.read (reinterpret_cast<char*>(&nelem), sizeof(LSize));
      is.read (reinterpret_cast<char*>(this->data()), nelem * sizeof(ValueType));
   }
  
    template <typename ValueType, StdSize NumDims, typename Allocator>
    size_t CArray<ValueType, NumDims, Allocator>::getSize(void) const
   {
      typedef boost::multi_array_types::size_type LSize;
      LSize nelem = this->num_elements();
      LSize ndim  = this->num_dimensions();
      const LSize * shape = this->shape();
      const ValueType * data = this->data();
      size_t ret ;
      ret=sizeof(ndim) ;
      for (LSize i = 0; i < ndim; i++ ) ret+=sizeof(shape[i]) ;
      ret+=sizeof(nelem) ;
      ret+=sizeof(ValueType)*nelem ;
      return ret ;
   }

   template <typename ValueType, StdSize NumDims, typename Allocator>
   bool CArray<ValueType, NumDims, Allocator>::toBuffer(CBufferOut& buffer) const
   {
      typedef boost::multi_array_types::size_type LSize;

      LSize nelem = this->num_elements();
      LSize ndim  = this->num_dimensions();
      const LSize* shape = this->shape();
      const ValueType* data = this->data();
      bool ret ;
      
      ret=buffer.put(ndim) ;
      for (LSize i = 0; i < ndim; i++ ) ret&=buffer.put(shape[i]) ;
      ret&=buffer.put(nelem) ;
      ret&=buffer.put(data,nelem) ;
      return ret ;
  }

   template <typename ValueType, StdSize NumDims, typename Allocator>
   bool CArray<ValueType, NumDims, Allocator>::fromBuffer(CBufferIn& buffer)
   {
      typedef boost::multi_array_types::size_type LSize;
      LSize ndim = 0, nelem = 0, temp = 0;
      std::vector<LSize> shape;
      bool ret ;
      
      ret=buffer.get(ndim) ;
      for (LSize i = 0; i < ndim; i++ )
      {
         ret&=buffer.get(temp) ;
         shape.push_back(temp);
      }
      this->resize(shape);
      ret&=buffer.get(nelem) ;
      ret&=buffer.get(this->data(),nelem) ;
      return ret ;
   }
   

   ///---------------------------------------------------------------
   
} // namespace xmlioserver

#endif // __XMLIO_CArray_impl__
