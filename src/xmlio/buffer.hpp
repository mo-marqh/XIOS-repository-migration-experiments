#ifndef __XMLIO_CBuffer__
#define __XMLIO_CBuffer__

/// xmlioserver headers ///
#include "xmlioserver_spl.hpp"
#include "exception.hpp"
#include "array.hpp"

namespace xmlioserver
{
   namespace comm
   {
      /// ////////////////////// Déclarations ////////////////////// ///
      class CBuffer
      {
         public :

         //--------------------------------------------------------------

            typedef enum buffer_data_type
            { TBOOL8 = 0, TINT32, TCHAR8, TFLOAT32, TDOUBLE64
            } CBufferDataType;

            typedef struct  buffer_data
            { CBufferDataType type; bool isArray; StdSize size; StdSize position;
            } CBufferData;

         //--------------------------------------------------------------

            /// Operateurs ///
            operator char * (void);
            char * operator[](StdSize position);

            /// Accesseurs ///
            StdSize getSize(StdSize position = 0) const;
            char * getData(StdSize position = 0) const;

            template <class T>
               inline StdSize getRequestedSize(T data) const;
            template <class T>
               inline StdSize getRequestedSize(ARRAY(T, 1) data) const;

            StdSize getNextDataPosition(StdSize position);
            static inline StdSize getDataHeaderSize(void) { return sizeof(CBufferDataType) + 2 * sizeof(StdSize) + sizeof(bool) ; }
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

            /// Mutateurs ///
            void setChar  (char     value, StdSize position) ;
            void setBool  (bool     value, StdSize position) ;
            void setFloat (float    value, StdSize position) ;
            void setDouble(double   value, StdSize position) ;
            void setInt   (long int value, StdSize position) ;

            void setString(const StdString & value, StdSize position) ;

            void setCharArray  (ARRAY(char,     1) value, StdSize position) ;
            void setBoolArray  (ARRAY(bool,     1) value, StdSize position) ;
            void setFloatArray (ARRAY(float,    1) value, StdSize position) ;
            void setDoubleArray(ARRAY(double,   1) value, StdSize position) ;
            void setIntArray   (ARRAY(long int, 1) value, StdSize position) ;

         //--------------------------------------------------------------

            virtual void clear(void) = 0;
            void updateBufferData(StdSize position);

            /// Sortie fichier binaire ///
            void printToBinaryFile (const StdString & filename);
            void printToBinaryStream (StdOStream & ostr);

            /// Sortie fichier ascii ///
            virtual void printToTextFile (const StdString & filename) = 0;
            virtual void printToTextStream (StdOStream & ostr) = 0;

            /// Destructeur ///
            virtual ~CBuffer(void);

         protected :

            /// Construteurs ///
            CBuffer(char * data, StdSize size);
            explicit CBuffer(StdSize size);
            CBuffer(const CBuffer & buffer);
            CBuffer(const CBuffer * const buffer);

            /// Accesseurs protégés ///
            void getBufferData(CBufferData & bufdata, StdSize position) const;

            template <class T>
               inline void getData(T & data, StdSize position) const;
            template <class T>
               inline void getDataArray(ARRAY(T, 1) data, StdSize position) const;

            /// Mutateurs protégés ///
            template <class T>
               inline void setData(const T & data, StdSize position);
            template <class T>
               inline void setDataArray(const ARRAY(T, 1) data, StdSize position);

            void setData(const char * data, StdSize size, StdSize position);
            void fillData(char data, StdSize size, StdSize position);

         private :

         //--------------------------------------------------------------

            /// Accesseurs privés ///
            void getData(char * data, StdSize size, StdSize position) const;
            template <class T>
               CBufferDataType getBufferDataType(void) const;

            /// Mutateurs privés ///
            void setBufferData(const CBufferData & bufdata, StdSize position);
            
         //--------------------------------------------------------------

            /// Propriétés privées ///
            StdSize size;
            char * idata;
            bool delIdata;

      }; // class CBuffer
   } // namespace comm
} // namespace xmlioserver

#endif // __XMLIO_CBuffer__
