#ifndef __XMLIO_CBuffer_impl__
#define __XMLIO_CBuffer_impl__

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////// Définitions (inline) /////////////////// ///

#define DATA_HEADER_SIZE  sizeof(CBufferDataType) + 2 * sizeof(StdSize) + sizeof(bool)

      template <class T>
         StdSize CBuffer::getRequestedSize(T data) const
      { return (DATA_HEADER_SIZE + sizeof (T)); }

      template <class T>
         StdSize CBuffer::getRequestedSize(ARRAY(T, 1) data) const
      { return (DATA_HEADER_SIZE + data->num_elements() * sizeof (T)); }

      template <>
         StdSize CBuffer::getRequestedSize(StdString data) const;

      //---------------------------------------------------------------

      template <class T>
         void CBuffer::getData(T & data, StdSize position) const
      {
         if (this->size < (position + this->getRequestedSize(data)))
            ERROR("CBuffer::getData(data, position)",
                   << " Buffer size <  size + position !");
         CBufferData bufdata;
         this->getBufferData(bufdata, position);
         if (bufdata.type != this->getBufferDataType<T>())
            ERROR("CBuffer::getData(data, position)", 
                  << "[ BufferDataType Read : " << bufdata.type                 << ", "
                  << ", BufferDataType T : "    << this->getBufferDataType<T>() << "] "
                  << " invalid type !");
         if (bufdata.isArray != false)
            ERROR("CBuffer::getData(data, position)",
                   << " type should be an array !");

         this->getData(reinterpret_cast<char*>(&data), bufdata.size , bufdata.position);
      }

      template <class T>
         void CBuffer::getDataArray(ARRAY(T, 1) data, StdSize position) const
      {
         CBufferData bufdata;
         this->getBufferData(bufdata, position);
         if (bufdata.type != this->getBufferDataType<T>())
            ERROR("CBuffer::getDataArray(data, position)", << " invalid type !");
         if (bufdata.isArray != true)
            ERROR("CBuffer::getDataArray(data, position)",
                  << " type should not be an array !");
         if (this->size < (position + (DATA_HEADER_SIZE + bufdata.size)))
            ERROR("CBuffer::getData<T>(data, size, position)",
                   << " Buffer size <  size + position !");
         data->resize(boost::extents[bufdata.size/sizeof(T)]);
         this->getData(reinterpret_cast<char*>(data->data()), bufdata.size , bufdata.position);
      }

      //---------------------------------------------------------------

      template <class T>
         void CBuffer::setData(const T & data, StdSize position)
      {
         if (this->size < (position + this->getRequestedSize(data)))
            ERROR("CBuffer::setData<T>(data, size, position)",
                   << " Buffer size <  size + position !");
         CBufferData bufdata;
         bufdata.type     = this->getBufferDataType<T>();
         bufdata.isArray  = false;
         bufdata.size     = sizeof(T);
         bufdata.position = position + DATA_HEADER_SIZE;
         this->setBufferData(bufdata, position);
         this->setData(reinterpret_cast<const char*>(&data), bufdata.size , bufdata.position);
      }

      template <class T>
         void CBuffer::setDataArray(const ARRAY(T, 1) data, StdSize position)
      {
         if (this->size < (position + this->getRequestedSize(data)))
            ERROR("CBuffer::setDataArray<T>(data, size, position)",
                   << " Buffer size <  size + position !");
         CBufferData bufdata;
         bufdata.type     = this->getBufferDataType<T>();
         bufdata.isArray  = true;
         bufdata.size     = data->num_elements() * sizeof (T);
         bufdata.position = position + DATA_HEADER_SIZE;

         this->setBufferData(bufdata, position);
         this->setData(reinterpret_cast<const char*>(data->data()),
                       bufdata.size , bufdata.position);
      }
      ///----------------------------------------------------------------

   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CBuffer_impl__
