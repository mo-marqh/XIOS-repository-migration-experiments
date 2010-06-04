#include "xmlio.hpp"

using namespace XMLIOSERVER;
using namespace XMLIOSERVER::XML;
using namespace std;

extern "C" void main_c_(void) ;

void main_c_ (void)
{
	try
	{
		XMLParser::CLASS_TEST();            
		// Code à executer ici
	}
	catch(const XMLIOException &exc)
	{  // Pour tout type d'exceptions, on note les informations sur la sortie paramétrée.
		ERROR(exc.displayText()); 
		// On retourne le code d'erreur en fin d'application pour traitements éventuels.
//		return (exc.code());
	}
      
//	return (0);
} 
