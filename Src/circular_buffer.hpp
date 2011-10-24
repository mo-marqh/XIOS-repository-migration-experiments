#ifndef __XMLIO_CCircularBuffer__
#define __XMLIO_CCircularBuffer__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "linear_buffer.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CCircularBuffer
         : public CBuffer
      {

            /// Définition de type ///
            typedef CBuffer SuperClass;

         public :

            /// Construteurs ///
            explicit CCircularBuffer(StdSize size = BUFFER_SERVER_SIZE);
            CCircularBuffer(const CCircularBuffer & cbuffer);
            CCircularBuffer(const CCircularBuffer * const cbuffer);

            /// Mutateurs ///
            virtual void clear(void);

            char * prepareNextData(StdSize data_size);
            StdSize prepareNextDataPosition(StdSize data_size);

            void appendRequest(const CLinearBuffer & brequest);

            void updateNbRequests(StdSize data_begin, StdSize data_end);

            /// Test ///
            bool hasRequest(void) const ;
            bool isAvailable(StdSize data_size) const;

            /// Accesseurs ///
            CLinearBuffer getNextRequest(void);
            StdSize getNumberOfRequest(void) const;

            StdSize getReadPosition(void) const;
            StdSize getWritePosition(void) const;
            StdSize getUnusedPosition(void) const;

            /// Sortie fichier ascii ///
            virtual void printToTextFile (const StdString & filename);
            virtual void printToTextStream (StdOStream & ostr);

            /// Destructeur ///
            virtual ~CCircularBuffer(void);

         private :

            /// Mutateurs privés ///
            void movePWrite(StdSize data_size);
            void movePRead(StdSize data_size);

            /// Accesseurs privés ///
            StdSize getNextRequestSize(void) const;

            /// Propriétés privées ///
            StdSize p_write, p_read, p_unused, nbrequest;

      }; // class CCircularBuffer
   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CCircularBuffer__
