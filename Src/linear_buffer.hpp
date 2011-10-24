#ifndef __XMLIO_CLinearBuffer__
#define __XMLIO_CLinearBuffer__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "buffer.hpp"

#define _NULL_ARG xmlioserver::comm::CLinearBuffer::NULL_ARG

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CLinearBuffer
         : public CBuffer
      {
            /// Définition de type ///
            typedef CBuffer SuperClass;

         public /* static */ :

            static long int * NULL_ARG;
            friend class CMPIManager;

         public :

            /// Construteurs ///

            explicit CLinearBuffer(StdSize size = BUFFER_CLIENT_SIZE);
            CLinearBuffer(char * data, StdSize size);
            CLinearBuffer(const CLinearBuffer & lbuffer);
            CLinearBuffer(const CLinearBuffer * const lbuffer);

            /// Accesseurs ///
            StdSize getUsedSize(void) const;
            StdSize getUnusedSize(void) const;

            StdSize getNumberOfStoredData(void) const;
            std::vector<StdSize> getPositionsOfStoredData(void) const;
            std::vector<StdSize> getSizesOfStoredData(void) const;
            std::vector<std::pair<CBufferDataType, bool> > getTypesOfStoredData(void) const;

         //--------------------------------------------------------------
            char     getChar  (StdSize position) const;
            bool     getBool  (StdSize position) const;
            float    getFloat (StdSize position) const;
            double   getDouble(StdSize position) const;
            long int getInt   (StdSize position) const;

            StdString getString(StdSize position) const;

            ARRAY(char,     1) getCharArray  (StdSize position) const;
            ARRAY(bool,     1) getBoolArray  (StdSize position) const;
            ARRAY(float,    1) getFloatArray (StdSize position) const;
            ARRAY(double,   1) getDoubleArray(StdSize position) const;
            ARRAY(long int, 1) getIntArray   (StdSize position) const;

         //--------------------------------------------------------------

            void getRequestInfos(StdSize position,
                     long int & managerId, long int & methodId, long int & nbargs);

         //--------------------------------------------------------------
            /// Mutateurs ///
            void appendChar  (char     value) ;
            void appendBool  (bool     value) ;
            void appendFloat (float    value) ;
            void appendDouble(double   value) ;
            void appendInt   (long int value) ;

            void appendString(const StdString & value) ;

            void appendCharArray  (ARRAY(char,     1) value) ;
            void appendBoolArray  (ARRAY(bool,     1) value) ;
            void appendFloatArray (ARRAY(float,    1) value) ;
            void appendDoubleArray(ARRAY(double,   1) value) ;
            void appendIntArray   (ARRAY(long int, 1) value) ;

         //--------------------------------------------------------------

            void appendRequestInfos(const long int & managerId,
                                    const long int & methodId,
                                    const long int & nbargs);

         //--------------------------------------------------------------
            virtual void clear(void);

            /// Test ///
            template <typename A1, typename A2, typename A3, typename A4>
               inline StdSize getRequestedSize(A1 * arg1 = NULL_ARG,
                                               A2 * arg2 = NULL_ARG,
                                               A3 * arg3 = NULL_ARG,
                                               A4 * arg4 = NULL_ARG) const;

            /// Sortie fichier ascii ///
            virtual void printToTextFile (const StdString & filename);
            virtual void printToTextStream (StdOStream & ostr);

            /// Destructeur ///
            virtual ~CLinearBuffer(void);

         protected :

            /// Accesseurs protégés ///
            template <class T>
               inline void getLData(T & data, StdSize position) const;
            template <class T>
               inline void getLDataArray(ARRAY(T, 1) data, StdSize position) const;

            /// Mutateurs protégés ///
            template <class T>
               inline void appendLData(const T & data);
            template <class T>
               inline void appendLDataArray(ARRAY(T, 1) data);

         private :

            /// Traitement ///
            void computeBufferData(void);

            /// Propriétés privées ///
            std::vector<StdSize> bdata;
            StdSize p_write;

      }; // class CLinearBuffer

      ///----------------------------------------------------------------

   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CLinearBuffer__
