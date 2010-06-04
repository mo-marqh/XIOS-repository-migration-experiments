#ifndef __XMLIO_EXCEPTION__
#define __XMLIO_EXCEPTION__ 

// Classes utilisées issues de la STL
using std::string;

// Classes utilisées issues de Poco
using Poco::Exception;

namespace XMLIOSERVER
{
   class XMLIOException : public Exception
   {
      public :
            
         XMLIOException(int _code): Exception(_code)                                                   
         { /* Ne rien faire de plus */ }                                                                     
         XMLIOException(const std::string& _msg, int _code): Exception(_msg, _code)                              
         { /* Ne rien faire de plus */ }                                                                        
         XMLIOException(const std::string& _msg, const std::string& _arg, int _code): Exception(_msg, _arg, _code)      
         { /* Ne rien faire de plus */ }                                                                        
         XMLIOException(const std::string& _msg, const Poco::Exception& _exc, int _code): Exception(_msg, _exc, _code)   
         { /* Ne rien faire de plus */ }   
         
                                                                              
         XMLIOException(const XMLIOException& _exc): Exception(_exc)                                                
         { /* Ne rien faire de plus */ }
         
         ~XMLIOException() throw()
         { /* Ne rien faire de plus */ }
         
         XMLIOException& operator = (const XMLIOException& _exc)                                             
         { Exception::operator = (_exc); return *this; }   
                                                                           
         virtual const char* name(void) const throw() { return ("XMLIO>XMLIOException"); }                                                                     
         virtual const char* className(void) const throw() { return (typeid(*this).name()); }      
                                                                     
         virtual Exception* clone(void) const {   return new XMLIOException(*this); }                                                                     
         virtual void rethrow(void) const { throw *this; }
               
   };// class XMLIOException
   
   class XMLIOUndefinedValueException : public XMLIOException
   {
      public :
         XMLIOUndefinedValueException(const std::string& _msg): XMLIOException(_msg, 1001) {}
         const char* name(void) const throw() { return ("XMLIO>UndefinedValueException"); }
            
   }; //class XMLIOUndefinedException
   
   class XMLIOStreamException : public XMLIOException
   {
      public :
         XMLIOStreamException(const std::string& _msg): XMLIOException(_msg, 1002) {}
         const char* name(void) const throw() { return ("XMLIO>StreamException"); }
            
   }; //class XMLIOStreamException
   
   class XMLParsingException : public XMLIOException
   {
      public :
         XMLParsingException(const std::string& _msg): XMLIOException(_msg, 1003) {}
         const char* name(void) const throw() { return ("XMLIO>XMLParsingException"); }
            
   }; //class XMLParsingException
   
   class XMLIOIncompatibeTypeException : public XMLIOException
   {
      public :
         XMLIOIncompatibeTypeException(const std::string& _msg): XMLIOException(_msg, 1003) {}
         const char* name(void) const throw() { return ("XMLIO>XMLIOIncompatibeTypeException"); }
            
   }; //class XMLIOIncompatibeTypeException
   
   // A compléter.
   
};// namespace XMLIOSERVER

#endif // __XMLIO_EXCEPTION__ 
