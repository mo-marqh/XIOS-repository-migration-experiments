#ifndef __XMLIO_XML_NODE__
#define __XMLIO_XML_NODE__ 

// Entêtes Poco DOM
#include <Poco/DOM/DOMParser.h>
#include <Poco/DOM/Document.h>
#include <Poco/DOM/Element.h>

#include <Poco/DOM/NamedNodeMap.h>

#include <Poco/DOM/AutoPtr.h>

// Entêtes Poco SAX
#include <Poco/SAX/InputSource.h>

// Utilisation de la STL
using std::string;

using std::pair;
using std::vector;
using std::deque;

using std::istream;
using std::ostream;
using std::ostringstream;
using std::ifstream;

// Utilisation de la biliothèque POCO
using Poco::XML::DOMParser;
using Poco::XML::InputSource;

using Poco::XML::Document;
using Poco::XML::Node;
using Poco::XML::Element;

using Poco::XML::NamedNodeMap;

using Poco::HashMap;

using Poco::XML::AutoPtr;

namespace XMLIOSERVER
{
   namespace XML
   {
      
      typedef HashMap<string, string> THashAttributes;
         
      // TODO Mettre des auto_ptr ici car gestion de la mémoire lamentable sans.
      typedef AutoPtr<Document> PDocument;
      typedef Node*    PNode;
         
      class XMLNode
      {
         public :
            
            XMLNode(const string& _rootName) : rootName(_rootName) 
            { /* Ne rien faire de plus */}          

            static XMLNode& CreateNode(XMLNode& node, istream& _istr, const string& _rootName = string("simulation"))
            {
               if ((_istr.good()))
               { // S'il est possible de lire le flux en entrée ...
                  InputSource src(_istr);
                  DOMParser parser;
                     
                  // On parse la source XML et on vérifie que le premier noeud (racine) est du type "Element"
                  // ... et à pour valeur la chaîne rootName.
                  node.pDoc = parser.parse(&src);
                  if (!(node.pDoc->documentElement()->nodeName().compare(_rootName)))
                  {
                     node.setCNode(node.pDoc->documentElement());
                     node.goToChildElement();
                  }
                  else
                  {
                     ostringstream oss;
                     oss << "L'élément racine doit avoir pour valeur <" << _rootName << "> (\"" <<  (node.pDoc->documentElement()->nodeName()) <<"\" lue)";
                     throw XMLParsingException(oss.str());
                  }    
               }
               else
                  throw XMLIOStreamException("Impossible de lire le flux en entrée pour le parsing XML !");
                  
               return (node);
            }
               
            string getElementName(void) const {return (this->getCNode()->nodeName());}
                
            bool goToNextElement(void) 
            {
               PNode nextElement = this->getCNode()->nextSibling();
              
               // On parcourt la liste des "siblings" jusqu'à trouver un élément quelconque.
               for(; ; nextElement = nextElement->nextSibling())
                  if (IsPtrNull(nextElement)) break;
                  else if (nextElement->nodeType() == 1)
                  {// Si l'un des noeuds est un élément...
                     this->setCNode(nextElement) ;
                     return (true);
                  } 
               return (false);
            }
             
            bool goToChildElement(void)
            {
               PNode nextElement = this->getCNode()->firstChild();
               
               // On parcourt la liste des enfants jusqu'à trouver un élément quelconque.
               if (!IsPtrNull(nextElement))
               {
                  for(; ; nextElement = nextElement->nextSibling())
                     if (IsPtrNull(nextElement)) break;
                     else if (nextElement->nodeType() == 1)
                     {// Si l'un des noeuds est un élément...
                        this->setCNode(nextElement) ;
                        return (true);
                     }
                  return (false);
               }
                
               return (false);
            }
             
            bool goToParentElement(void)
            { 
               // Pas de retour au parent si on est à la racine.
               if (!(this->getElementName().compare(rootName))) return (false);
               this->setCNode(this->getCNode()->parentNode());
               return (true);
            }
             
            bool getAttributes(THashAttributes& attributes) const
            {
                
               if(!this->getCNode()->hasAttributes()) return (false);
               AutoPtr<NamedNodeMap> map = this->getCNode()->attributes();
              
               for(unsigned int i = 0; i< map->length(); i++)
                  attributes[map->item(i)->nodeName()] = map->item(i)->nodeValue();

               return (true);
            }
            
            ~XMLNode() 
            { /* Ne rien faire de plus */ }
            
         protected :
         
            PNode getCNode(void) const {return (this->cNode); }
            void setCNode(PNode other) { this->cNode = other; }
            
            static bool IsPtrNull(PNode ptr) {return (ptr==NULL);}
            
         private :
            PDocument pDoc; 
            PNode  cNode;
            
            string rootName;      
         
      };// class XMLNode
      
         }; // namespace XML

};// namespace XMLIOSERVER


#endif // __XMLIO_XML_NODE__ 
