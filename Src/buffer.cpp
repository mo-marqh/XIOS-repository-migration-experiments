#include "buffer.hpp"

#include "mpi_manager.hpp"
#include "buffer_impl.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CBuffer::CBuffer(StdSize size)
         : size(size), delIdata(true)
      {
         this->idata = new char[size]();
         //CMPIManager::AllocMem(&idata, size);
      }

      CBuffer::CBuffer(char * data, StdSize size)
         : size(size), delIdata(false)
      {
         this->idata = data;
      }
      
      CBuffer::CBuffer(const CBuffer & buffer)
         : size(buffer.size), delIdata(true)
      {
         this->idata = new char[size]();
         std::copy (buffer.idata, buffer.idata+size, this->idata);
      }
      
      CBuffer::CBuffer(const CBuffer * const buffer)
         : size(buffer->size), delIdata(true)
      {
         this->idata = new char[size]();
         std::copy (buffer->idata, buffer->idata+size, this->idata);       
      }

      CBuffer::~CBuffer(void)
      {
         if (delIdata) delete [] this->idata;
         else this->fillData('\0', this->getSize(), 0);
         //CMPIManager::FreeMem(idata);
      }

      ///--------------------------------------------------------------

      CBuffer::operator char * (void)
      { return (this->getData()); }

      char * CBuffer::operator[](StdSize position)
      { return (this->getData(position)); }

      //---------------------------------------------------------------

      StdSize CBuffer::getSize(StdSize position) const
      {
         if (position > this->size)
            ERROR("CBuffer::getSize(position)",
                  << " Buffer size <  size + position !");
         return (this->size - position);
      }

      char * CBuffer::getData(StdSize position) const
      {
         if (position > this->size)
            ERROR("CBuffer::getData(position)",
                   << " Buffer size < position !");
         return &(this->idata[position]);
      }

      //---------------------------------------------------------------

      void CBuffer::getData(char * data, StdSize size, StdSize position) const
      {
         if (this->size < (size + position))
            ERROR("CBuffer::getData(data, size, position)",
                   << " Buffer size <  size + position !");
         std::copy(this->getData(position), this->getData(position + size), data);
      }
      
      //---------------------------------------------------------------
      
      void CBuffer::setData(const char * data, StdSize size, StdSize position)
      {
         if (this->size < (size + position))
            ERROR("CBuffer::getData(data, size, position)",
                   << " Buffer size <  size + position !");
         std::copy(&(data[0]), &(data[size]), this->getData(position));
      }

      //---------------------------------------------------------------

#define BufferDataTypeGetter(type, type_enum)\
   template <> CBuffer::CBufferDataType \
      CBuffer::getBufferDataType<type>(void) const { return (type_enum); }; \
   template <> CBuffer::CBufferDataType \
      CBuffer::getBufferDataType<ARRAY(type, 1)>(void) const { return (type_enum); };

      BufferDataTypeGetter(bool, TBOOL8);
      BufferDataTypeGetter(char, TCHAR8);
      BufferDataTypeGetter(float, TFLOAT32);
      BufferDataTypeGetter(double, TDOUBLE64);
      BufferDataTypeGetter(long int, TINT32);

#undef BufferDataTypeGetter


      //---------------------------------------------------------------

      void CBuffer::getBufferData(CBufferData & bufdata, StdSize position) const
      {
         StdSize currposition = position;
         this->getData( reinterpret_cast<char*>(&(bufdata.type))
                      , sizeof(CBufferDataType), currposition);
         this->getData( reinterpret_cast<char*>(&(bufdata.isArray))
                      , sizeof(bool), currposition += sizeof(CBufferDataType));
         this->getData( reinterpret_cast<char*>(&(bufdata.size))
                      , sizeof(StdSize), currposition += sizeof(bool));
         this->getData( reinterpret_cast<char*>(&(bufdata.position))
                      , sizeof(StdSize), currposition += sizeof(StdSize));
      }

      void CBuffer::setBufferData(const CBufferData & bufdata, StdSize position)
      {
         StdSize currposition = position;
         this->setData( reinterpret_cast<const char*>(&(bufdata.type))
                      , sizeof(CBufferDataType), currposition);
         this->setData( reinterpret_cast<const char*>(&(bufdata.isArray))
                      , sizeof(bool), currposition += sizeof(CBufferDataType));
         this->setData( reinterpret_cast<const char*>(&(bufdata.size))
                      , sizeof(StdSize), currposition += sizeof(bool));
         this->setData( reinterpret_cast<const char*>(&(bufdata.position))
                      , sizeof(StdSize), currposition += sizeof(StdSize));
      }

      //---------------------------------------------------------------

#define CBufferGetter(type, Type)       \
   type CBuffer::get##Type(StdSize position) const \
   { type retvalue; this->getData<type>(retvalue, position); return (retvalue); }


      CBufferGetter(char     , Char)
      CBufferGetter(bool     , Bool)
      CBufferGetter(float    , Float)
      CBufferGetter(double   , Double)
      CBufferGetter(long int , Int)

#undef CBufferGetter

      //---------------------------------------------------------------

#define CBufferArrayGetter(type, Type) \
   ARRAY(type, 1) CBuffer::get##Type##Array(StdSize position) const \
   {  ARRAY_CREATE(retvalue, type, 1, [1]);                         \
      this->getDataArray<type>(retvalue, position);                 \
      return (retvalue); }

      CBufferArrayGetter(char     , Char)
      CBufferArrayGetter(bool     , Bool)
      CBufferArrayGetter(float    , Float)
      CBufferArrayGetter(double   , Double)
      CBufferArrayGetter(long int , Int)

#undef CBufferArrayGetter

      StdString CBuffer::getString(StdSize position) const
      {
         ARRAY(char, 1) array = this->getCharArray(position);
         return (StdString(array->data(), array->num_elements()));
      }

      //---------------------------------------------------------------

#define CBufferSetter(type, Type) \
      void  CBuffer::set##Type(type value, StdSize position) \
      { this->setData(value, position); }

      CBufferSetter(char     , Char)
      CBufferSetter(bool     , Bool)
      CBufferSetter(float    , Float)
      CBufferSetter(double   , Double)
      CBufferSetter(long int , Int)

#undef CBufferSetter

      //---------------------------------------------------------------

#define CBufferArraySetter(type, Type) \
      void  CBuffer::set##Type##Array(ARRAY(type, 1) value, StdSize position) \
      { this->setDataArray(value, position); }

      CBufferArraySetter(char     , Char)
      CBufferArraySetter(bool     , Bool)
      CBufferArraySetter(float    , Float)
      CBufferArraySetter(double   , Double)
      CBufferArraySetter(long int , Int)

#undef CBufferArraySetter

      void CBuffer::setString(const StdString & value, StdSize position)
      {
         ARRAY_CREATE(arr, char, 1, [value.size()]);
         std::copy(value.data(), &(value.data()[value.size()]), arr->data());
         this->setCharArray(arr, position);
      }

      //---------------------------------------------------------------

      void CBuffer::printToBinaryFile (const StdString & filename)
      {
         StdOFStream ofs(filename.c_str());
         this->printToBinaryStream(ofs);
         ofs.close();
      }
      
      //---------------------------------------------------------------
      
      void CBuffer::printToBinaryStream (StdOStream & ostr)
      {  
         ostr.write (this->getData(), this->getSize()); 
      }
      
      //---------------------------------------------------------------
      
      StdSize CBuffer::getNextDataPosition(StdSize position)
      {
         CBufferData  bufdata;
         this->updateBufferData(position);
         this->getBufferData(bufdata, position);
         return (bufdata.size + bufdata.position);
      }

      //---------------------------------------------------------------

      template <>
         StdSize CBuffer::getRequestedSize(StdString data) const
      { 
         return (DATA_HEADER_SIZE + data.size() * sizeof (char)); 
      }
      
      //---------------------------------------------------------------
      
      void CBuffer::updateBufferData(StdSize position)
      {
         CBufferData bufdata;
         this->getBufferData(bufdata, position);
         bufdata.position = position + DATA_HEADER_SIZE;
         this->setBufferData(bufdata, position);
      }
      
      //---------------------------------------------------------------
      
      void CBuffer::fillData(char data, StdSize size, StdSize position)
      {
         if (this->size < (size + position))
            ERROR("CBuffer::getData(data, size, position)",
                   << " Buffer size <  size + position !");
         std::fill_n(this->getData(position), size, data);
      }
      
      ///--------------------------------------------------------------

   } // namespace tree
} // namespace xmlioserver
