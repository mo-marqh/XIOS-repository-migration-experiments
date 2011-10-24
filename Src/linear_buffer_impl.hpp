#ifndef __XMLIO_CLinearBuffer_impl__
#define __XMLIO_CLinearBuffer_impl__

#include "buffer_impl.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////// Définitions (inline) /////////////////// ///

      template <class T>
         void CLinearBuffer::getLData(T & data, StdSize position) const
      {
         if (position >= bdata.size())
            ERROR("CLinearBuffer::getData(position)",
                  << " invalid position !");
         SuperClass::getData(data, this->bdata[position]);
      }

      template <class T>
          void CLinearBuffer::getLDataArray(ARRAY(T, 1) data, StdSize position) const
      {
         if (position >= bdata.size())
            ERROR("CLinearBuffer::getDataArray(position)",
                  << " invalid position !");
         SuperClass::getDataArray(data, this->bdata[position]);
      }

      //---------------------------------------------------------------

      template <class T>
         void CLinearBuffer::appendLData(const T & data)
      {
         this->bdata.push_back(this->p_write);
         SuperClass::setData(data, this->p_write);
         this->p_write += SuperClass::getRequestedSize(data);
      }

      template <class T>
         void CLinearBuffer::appendLDataArray(ARRAY(T, 1) data)
      {
         this->bdata.push_back(this->p_write);
         SuperClass::setDataArray(data, this->p_write);
         this->p_write += SuperClass::getRequestedSize(data);
      }
      //---------------------------------------------------------------

      template <typename A1, typename A2, typename A3, typename A4>
         StdSize CLinearBuffer::getRequestedSize
          ( A1 * arg1, A2 * arg2, A3 * arg3, A4 * arg4) const
      {
         StdSize retvalue = 0;
         long int nbarg   = 0;
         retvalue += 3 * SuperClass::getRequestedSize(nbarg);

         if (arg1 != NULL) retvalue += SuperClass::getRequestedSize(*arg1);
         if (arg2 != NULL) retvalue += SuperClass::getRequestedSize(*arg2);
         if (arg3 != NULL) retvalue += SuperClass::getRequestedSize(*arg3);
         if (arg4 != NULL) retvalue += SuperClass::getRequestedSize(*arg4);
         return (retvalue);
      }

      ///----------------------------------------------------------------

   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CLinearBuffer_impl__
