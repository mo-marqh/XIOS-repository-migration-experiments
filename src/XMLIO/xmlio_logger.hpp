#ifndef __XMLIO_LOGGER__
#define __XMLIO_LOGGER__ 

// Entête Poco logging
#include <Poco/Logger.h>
#include <Poco/PatternFormatter.h>
#include <Poco/FormattingChannel.h>
#include <Poco/ConsoleChannel.h>
#include <Poco/FileChannel.h>
#include <Poco/Message.h>

#include <Poco/AutoPtr.h>


// Classes utilisées issues de Poco
using Poco::Logger;
using Poco::PatternFormatter;
using Poco::FormattingChannel;
using Poco::ConsoleChannel;
using Poco::FileChannel;
using Poco::Message;

using Poco::AutoPtr;

namespace XMLIOSERVER
{
   class ILogger
   {
      public :
      
         ILogger(const string& _fileLogName, bool _withConsole = true)
         {
            // Perte mémoire faible ici (/ still reachable: 84 bytes in 2 blocks /)
   
            // TODO Créer une sortie fichier.
            AutoPtr<PatternFormatter> pf = new PatternFormatter("[%Y-%m-%d %H:%M:%S] %t");
            AutoPtr<ConsoleChannel> cc = new ConsoleChannel();
            AutoPtr<FormattingChannel> pFCConsole = new FormattingChannel(pf);
            pFCConsole->setChannel(cc);
            pFCConsole->open();            
            Logger::create("ConsoleLogger", pFCConsole, Message::PRIO_INFORMATION);
         }

         static Logger & GetConsoleLogger(void) {return (Logger::get("ConsoleLogger"));}   
         
         ~ILogger(void) 
         { /* Ne rien faire de plus */ }   
               
   }; // class XMLIOLogger
   
   // Initialisation de la classe de Logging
   static ILogger LOGGER("xmlio.log");

#define   ERROR(MSG)   (ILogger::GetConsoleLogger().error(MSG))
#define   WARNING(MSG)   (ILogger::GetConsoleLogger().warning(MSG))
#define   INFO(MSG)   (ILogger::GetConsoleLogger().information(MSG))
// A compléter.



/************* POUR MEMOIRE **********************
#include <stdio.h> 
#include <execinfo.h> 
#include <signal.h> 
#include <stdlib.h> 
 
void handler(int sig) { 
  void *array[10]; 
  size_t size; 
  // get void*'s for all entries on the stack 
  size = backtrace(array, 10); 
  // print out all the frames to stderr 
  fprintf(stderr, "Error: signal %d:\n", sig); 
  backtrace_symbols_fd(array, size, 2); 
  exit(1); 
} 
void baz() { 
 int *foo = (int*)-1; // make a bad pointer 
  printf("%d\n", *foo);       // causes segfault 
} 
void bar() { baz(); } 
void foo() { bar(); } 
 
int main(int argc, char **argv) { 
  signal(SIGSEGV, handler);   // install our handler 
  foo(); // this will call foo, bar, and baz.  baz segfaults. 
} 
 
 ***************************************************************/
   
}; // namespace XMLIOSERVER

#endif // __XMLIO_LOGGER__  
