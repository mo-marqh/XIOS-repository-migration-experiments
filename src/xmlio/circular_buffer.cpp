#include "circular_buffer.hpp"

#include "linear_buffer_impl.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Définitions ////////////////////// ///
      CCircularBuffer::CCircularBuffer(StdSize size)
         : SuperClass(size)
         , p_write(0), p_read(0), p_unused(0)
         , nbrequest(0)
      { /* Ne rien faire de plus */ }

      CCircularBuffer::CCircularBuffer(const CCircularBuffer & cbuffer)
         : SuperClass(cbuffer)
         , p_write(cbuffer.p_write), p_read(cbuffer.p_read), p_unused(cbuffer.p_unused)
         , nbrequest(cbuffer.nbrequest)
      { /* Ne rien faire de plus */ }
      
      CCircularBuffer::CCircularBuffer(const CCircularBuffer * const cbuffer)
         : SuperClass(cbuffer)
         , p_write(cbuffer->p_write), p_read(cbuffer->p_read), p_unused(cbuffer->p_unused)
         , nbrequest(cbuffer->nbrequest)
      { /* Ne rien faire de plus */ }

      CCircularBuffer::~CCircularBuffer(void)
      { /* Ne rien faire de plus */ }

      ///--------------------------------------------------------------

      void CCircularBuffer::clear(void)
      {
         this-> p_write  = 0;
         this-> p_read   = 0;
         this-> p_unused = 0;
      }

      //-------------------------------------------------------------

      StdSize CCircularBuffer::getNextRequestSize(void) const
      {
         if (!this->hasRequest())
            ERROR("CCircularBuffer::getNextRequestSize()",
                  << " invalid call !");
         long int nbargs  = 0;
         StdSize startpos = this-> p_read;
         StdSize currsize = 2 * SuperClass::getRequestedSize(nbargs);
         nbargs    = SuperClass::getInt(startpos + currsize);
         currsize += SuperClass::getRequestedSize(nbargs);
         
         for (long int i = 0; i < nbargs; i++)
         {
            CBufferData bufdata;
            SuperClass::getBufferData(bufdata, startpos + currsize);
            currsize += (bufdata.size + DATA_HEADER_SIZE);
         }
         return (currsize);
      }
      
      //-------------------------------------------------------------
      
      CLinearBuffer CCircularBuffer::getNextRequest(void)
      {
         StdSize startpos = this-> p_read;
         StdSize currsize = this->getNextRequestSize();
        
         this->movePRead(currsize);
         this->nbrequest--;
         //std::cout <<  this->nbrequest << std::endl;
        
         return (CLinearBuffer(SuperClass::getData(startpos), currsize));
      }

      //-------------------------------------------------------------

      void CCircularBuffer::appendRequest(const CLinearBuffer & brequest)
      {
         StdSize usedsize   = brequest.getUsedSize();
         if (!this->isAvailable(usedsize))
            ERROR("CCircularBuffer::appendRequest(brequest)",
                  << " invalid call !");
         StdSize data_begin = this->prepareNextDataPosition(usedsize);
         SuperClass::setData(brequest.getData(), usedsize, data_begin);
         this->updateNbRequests(data_begin, data_begin + usedsize);
      }

      //-------------------------------------------------------------

      char * CCircularBuffer::prepareNextData(StdSize data_size)
      {
         return (SuperClass::getData(this->prepareNextDataPosition(data_size)));
      }
      
      //-------------------------------------------------------------

      StdSize CCircularBuffer::prepareNextDataPosition(StdSize data_size)
      {
         StdSize startpos = this-> p_write;
         if (!this->isAvailable(data_size))
            ERROR("CCircularBuffer::prepareNextDataPosition(data_size)",
                  << " invalid call !");
         if ((this-> p_write + data_size) > SuperClass::getSize())
            startpos = 0;

         this->movePWrite(data_size);
         return (startpos);
      }

      //-------------------------------------------------------------

      void CCircularBuffer::movePRead(StdSize data_size)
      {
         this-> p_read += data_size;
         if ((this-> p_read == this-> p_unused) &&
             (this-> p_read == this-> p_write))
         {
            this->clear();
            return;
         }
         
         if (this-> p_read == this-> p_unused)
         {
            this-> p_unused = this-> p_write;
            this-> p_read   = 0;
            return;
         }
         if (this-> p_read == this-> p_write)
         {
            this->clear();
         }
      }
      
      //-------------------------------------------------------------
      
      void CCircularBuffer::movePWrite(StdSize data_size)
      {
         if ((this-> p_write + data_size) > SuperClass::getSize())
         {
            this-> p_unused = this-> p_write;
            this-> p_write  = data_size;
            if (this-> p_read < (data_size))
               ERROR("CCircularBuffer::movePWrite(data_size)",
                     << " invalid position 1 !");
         }
         else
         {
            if ((this-> p_read > this-> p_write) &&
                (this-> p_read < (this-> p_write + data_size)))
               ERROR("CCircularBuffer::movePWrite(data_size)",
                     << " invalid position 2 !");
            this-> p_write += data_size;
            if (this->p_read < this->p_write)
               this-> p_unused = this-> p_write;
         }
      }

      //-------------------------------------------------------------

      bool CCircularBuffer::hasRequest(void) const
      { 
         return (this->nbrequest != 0); 
      }
      
      //-------------------------------------------------------------
      
      bool CCircularBuffer::isAvailable(StdSize data_size) const
      {
         if (this->p_write == this->p_unused)
            return (((SuperClass::getSize() - this->p_write) >= data_size) ||
                   (this->p_read >= data_size));
         else
            return ((this->p_read - this->p_write) >= data_size);
      }
      //---------------------------------------------------------------

      void CCircularBuffer::printToTextFile (const StdString & filename)
      {
         StdOFStream ofs(filename.c_str());
         this->printToTextStream(ofs);
         ofs.close();
      }
      
      //-------------------------------------------------------------
      
      void CCircularBuffer::printToTextStream (StdOStream & ostr)
      {
         StdSize _p_write   = p_write,
                 _p_read    = p_read,
                 _p_unused  = p_unused,
                 _nbrequest = nbrequest;

         while (this->hasRequest())
         {
            this->getNextRequest().printToTextStream(ostr);
            ostr << std::endl;
         }

         p_write   = _p_write;
         p_read    = _p_read;
         p_unused  = _p_unused;
         nbrequest = _nbrequest;
      }

      //---------------------------------------------------------------

      StdSize CCircularBuffer::getNumberOfRequest(void) const
      { 
         return (this->nbrequest); 
      }

      //---------------------------------------------------------------

      StdSize CCircularBuffer::getReadPosition(void) const
      { 
         return (this->p_read); 
      }

      //---------------------------------------------------------------

      StdSize CCircularBuffer::getWritePosition(void) const
      { 
         return (this->p_write); 
      }

      //---------------------------------------------------------------

      StdSize CCircularBuffer::getUnusedPosition(void) const
      { 
         return (this->p_unused); 
      }

      //---------------------------------------------------------------

      void CCircularBuffer::updateNbRequests(StdSize data_begin, StdSize data_end)
      {
         StdSize position = data_begin;        
         while (position != data_end)
         {
            this->nbrequest++;
            position = SuperClass::getNextDataPosition(position); // manager id           
            position = SuperClass::getNextDataPosition(position); // method id           
            SuperClass::updateBufferData(position);
            long int nbarg = SuperClass::getInt(position);
            position = SuperClass::getNextDataPosition(position);
            for (long int i = 0; i < nbarg; i++)
               position = SuperClass::getNextDataPosition(position);
               
            if (position > data_end)
              ERROR("CCircularBuffer::updateNbRequests(StdSize data_begin, StdSize data_end)",
                     << "[ position courante" << position
                     << ", fin de traitement" << data_end << " ] "
                     << "Impossible de mettre à jour la liste des requêtes !");
         }
      }

      ///------------------------------------------------------------
   } // namespace tree
} // namespace xmlioserver

