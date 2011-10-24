#include "linear_buffer.hpp"

#include "linear_buffer_impl.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CLinearBuffer::CLinearBuffer(char * data, StdSize size)
         : SuperClass(data, size), bdata(), p_write(size)
      { this->computeBufferData(); }

      CLinearBuffer::CLinearBuffer(StdSize size)
         : SuperClass(size), bdata(), p_write(0)
      { /* Ne rien faire de plus */ }
      
      CLinearBuffer::CLinearBuffer(const CLinearBuffer & lbuffer)
         : SuperClass(lbuffer), bdata(lbuffer.bdata), p_write(lbuffer.p_write)
      { /* Ne rien faire de plus */ }
      
      CLinearBuffer::CLinearBuffer(const CLinearBuffer * const lbuffer)
         : SuperClass(lbuffer), bdata(lbuffer->bdata), p_write(lbuffer->p_write)      
      { /* Ne rien faire de plus */ }
      
      CLinearBuffer::~CLinearBuffer(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      long int * CLinearBuffer::NULL_ARG = NULL;

      //---------------------------------------------------------------

      void CLinearBuffer::clear(void)
      { 
         this->bdata.clear();
         //SuperClass::fillData('\0', SuperClass::getSize(), 0);
         this->p_write = 0;
      }
      
      //---------------------------------------------------------------
      
      StdSize CLinearBuffer::getUsedSize(void) const
      { 
         return (this->p_write); 
      }
      
      //---------------------------------------------------------------
      
      StdSize CLinearBuffer::getUnusedSize(void) const
      {
          return (SuperClass::getSize() - this->getUsedSize()); 
      }

      //---------------------------------------------------------------

#define CLinearBufferGetter(type, Type) \
   type CLinearBuffer::get##Type(StdSize position) const     \
   {                                                         \
      if (position >= bdata.size())                          \
         ERROR("CLinearBuffer::get"#Type"(position)",        \
               << " invalid position !");                    \
      return (SuperClass::get##Type(this->bdata[position])); \
   }

      CLinearBufferGetter(char     , Char)
      CLinearBufferGetter(bool     , Bool)
      CLinearBufferGetter(float    , Float)
      CLinearBufferGetter(double   , Double)
      CLinearBufferGetter(long int , Int)

#undef CLinearBufferGetter

      //---------------------------------------------------------------

#define CLinearBufferArrayGetter(type, Type) \
   ARRAY(type, 1) CLinearBuffer::get##Type##Array(StdSize position) const \
   {                                                                      \
      if (position >= bdata.size())                                       \
         ERROR("CLinearBuffer::get"#Type"Array(position)",                \
               << " invalid position !");                                 \
      return (SuperClass::get##Type##Array(this->bdata[position]));       \
   }

      CLinearBufferArrayGetter(char     , Char)
      CLinearBufferArrayGetter(bool     , Bool)
      CLinearBufferArrayGetter(float    , Float)
      CLinearBufferArrayGetter(double   , Double)
      CLinearBufferArrayGetter(long int , Int)

#undef CLinearBufferArrayGetter

      StdString CLinearBuffer::getString(StdSize position) const
      { return (SuperClass::getString(this->bdata[position])); }

      //---------------------------------------------------------------

#define CLinearBufferSetter(type, Type)                        \
      void  CLinearBuffer::append##Type(type value)            \
      {                                                        \
         this->bdata.push_back(this->p_write);                 \
         SuperClass::set##Type(value, this->p_write);          \
         this->p_write += SuperClass::getRequestedSize(value); \
      }

      CLinearBufferSetter(char     , Char)
      CLinearBufferSetter(bool     , Bool)
      CLinearBufferSetter(float    , Float)
      CLinearBufferSetter(double   , Double)
      CLinearBufferSetter(long int , Int)

#undef CLinearBufferSetter

      //---------------------------------------------------------------

#define CLinearBufferArraySetter(type, Type) \
      void  CLinearBuffer::append##Type##Array(ARRAY(type, 1) value) \
      {                                                              \
         this->bdata.push_back(this->p_write);                       \
         SuperClass::set##Type##Array(value, this->p_write);         \
         this->p_write += SuperClass::getRequestedSize(value);       \
      }

      CLinearBufferArraySetter(char     , Char)
      CLinearBufferArraySetter(bool     , Bool)
      CLinearBufferArraySetter(float    , Float)
      CLinearBufferArraySetter(double   , Double)
      CLinearBufferArraySetter(long int , Int)

#undef CLinearBufferArrayAppend

      void CLinearBuffer::appendString(const StdString & value)
      {
         ARRAY_CREATE(arr, char, 1, [value.size()]);
         std::copy(value.data(), &(value.data()[value.size()]), arr->data());
         this->appendCharArray(arr);
      }

      //---------------------------------------------------------------

      StdSize CLinearBuffer::getNumberOfStoredData(void) const
      { 
         return (this->bdata.size()); 
      }
      
      //---------------------------------------------------------------
      
      std::vector<StdSize> CLinearBuffer::getPositionsOfStoredData(void) const
      {
         std::vector<StdSize> retvalue;
         std::vector<StdSize>::const_iterator it = this->bdata.begin(), end = this->bdata.end();
         for (;it != end; it++)
         {
            StdSize pos = *it;
            CBufferData bufdata;
            SuperClass::getBufferData(bufdata, pos);
            retvalue.push_back(bufdata.position);
         }

         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      std::vector<StdSize> CLinearBuffer::getSizesOfStoredData(void) const
      {
         std::vector<StdSize> retvalue;
         std::vector<StdSize>::const_iterator it = this->bdata.begin(), end = this->bdata.end();
         for (;it != end; it++)
         {
            StdSize pos = *it;
            CBufferData bufdata;
            SuperClass::getBufferData(bufdata, pos);
            retvalue.push_back(bufdata.size);
         }
         return (retvalue);
      }
      
      //---------------------------------------------------------------
      
      std::vector<std::pair<CBuffer::CBufferDataType, bool> >
         CLinearBuffer::getTypesOfStoredData(void) const
      {
         std::vector<std::pair<CBufferDataType, bool> > retvalue;
         std::vector<StdSize>::const_iterator it = this->bdata.begin(), end = this->bdata.end();
         for (;it != end; it++)
         {
            StdSize pos = *it;
            CBufferData bufdata;
            SuperClass::getBufferData(bufdata, pos);
            retvalue.push_back(std::make_pair(bufdata.type, bufdata.isArray));
         }

         return (retvalue);
      }

      //---------------------------------------------------------------

      void CLinearBuffer::computeBufferData(void)
      {

         CBufferData bufdata; this->clear();
         StdSize total_size = SuperClass::getSize();
         int i = 0;
         while ((1000 >= i++) && (p_write != total_size))
         {
            bdata.push_back(this->p_write);
            SuperClass::updateBufferData(this->p_write);
            SuperClass::getBufferData(bufdata, this->p_write);
            this->p_write = (bufdata.size + bufdata.position);
         }
      }

      //---------------------------------------------------------------

      void CLinearBuffer::appendRequestInfos(const long int & managerId,
                                             const long int & methodId,
                                             const long int & nbargs)
      {
         this->appendInt(managerId);
         this->appendInt(methodId);
         this->appendInt(nbargs);
      }

      void CLinearBuffer::getRequestInfos(StdSize position,
                           long int & managerId, long int & methodId, long int & nbargs)
      {
         managerId = this->getInt(position);
         methodId  = this->getInt(position+1);
         nbargs    = this->getInt(position+2);
      }


      //---------------------------------------------------------------

      void CLinearBuffer::printToTextFile (const StdString & filename)
      {
         StdOFStream ofs(filename.c_str());
         this->printToTextStream(ofs);
         ofs.close();
      }

      void CLinearBuffer::printToTextStream (StdOStream & ostr)
      { // A améliorer ....
         StdSize position = 0;
         typedef std::pair<CBufferDataType, bool> pairBufBool;
         std::vector<pairBufBool> vect = this->getTypesOfStoredData();
         ostr << "|";
         
         std::vector<pairBufBool>::const_iterator it = vect.begin(), end = vect.end();
         for (;it != end; it++)
         {
            pairBufBool elm = *it;
            ostr << "|";
            if (!elm.second)
            {
               switch (elm.first)
               {
                  case (TBOOL8):    ostr << this->getBool(position);
                     break;
                  case (TINT32):    ostr << this->getInt(position);
                     break;
                  case (TCHAR8):    ostr << this->getChar(position);
                     break;
                  case (TFLOAT32):  ostr << this->getFloat(position);
                     break;
                  case (TDOUBLE64): ostr << this->getDouble(position);
                     break;
                  default :
                     return;
               }
            }
            else
            {
               switch (elm.first)
               {
                  case (TBOOL8):    ostr << this->getBoolArray(position);
                     break;
                  case (TINT32):    ostr << this->getIntArray(position);
                     break;
                  case (TCHAR8):    ostr << this->getCharArray(position);
                     break;
                  case (TFLOAT32):  ostr << this->getFloatArray(position);
                     break;
                  case (TDOUBLE64): ostr << this->getDoubleArray(position);
                     break;
                  default :
                     return;
               }
            }
            position++;
         }
      }

      ///--------------------------------------------------------------

   } // namespace tree
} // namespace xmlioserver

