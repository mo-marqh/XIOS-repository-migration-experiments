#ifndef __XMLIO_XML_PARSER__
#define __XMLIO_XML_PARSER__ 

// Utilisation de la STL
using std::string;

namespace XMLIOSERVER
{
   namespace XML
   {          
      using XMLIOSERVER::Context;
      
      class XMLParser
      {
         public : 
         
            static void Parse(XMLNode& _node)
            {
               THashAttributes attributes;
                              
               do {                
                 // Traitement de l'identifiant
                  _node.getAttributes(attributes);
                            
                  if (attributes.end() == attributes.find("id"))
                  { WARNING("Le context ne sera pas traité car il n'est pas identifié !"); continue; }
                  
                  if( Context::HasObject(attributes["id"]))
                  { WARNING("Le context ne sera pas traité car il existe déjà un autre context possédant le même nom !"); continue; }
                  
                  Context::SetCurrentContext(attributes["id"]);
                  Context& context = Context::CreateObject(attributes["id"]);
                  
                  context.parse(_node);
                  attributes.clear();
                  
               } while (_node.goToNextElement());               
            } 
            
            static void FreeMemory(void)
            {
               HashMap<string, StrHashMap<Context> > &AllListContext = Context::GetAllListObject();
               
               for (HashMap<string, StrHashMap<Context> >::Iterator it = AllListContext.begin(); it != AllListContext.end(); it++)
               {
                  string& id = (*it).first;
                  StrHashMap<Context>& sc = (*it).second;
                  // BUG AllListContext contient une StrHashMap<Context> liée à l'identifiant "" d'où la vérification ci-dessous.
                  if (id.size()>0) delete sc[id];
               }
            }
            
            static bool CLASS_TEST(const string& file = string("/local/testsuite/iodef_test.xml"), ostream& log = std::clog)
            {
               ifstream istr( file.c_str() , ifstream::in );
               XMLNode node("simulation");
               XMLNode::CreateNode(node, istr, "simulation");
               XMLParser::Parse(node);
               log << "Nombre de Contexts listés : " << Context::GetCurrentListObject().getSize() << " contre 1 attendus."<< std::endl;
               log << "Nombre de FieldGroups listés : " << FieldGroup::GetCurrentListObject().getSize() << " contre 5 attendus."<< std::endl;
               log << "Description du champs votkeavt : " << Field::GetObject("votkeavt").description << " contre \"Vertical Eddy Diffusivity\" attendus."<< std::endl;
               
               log << "Test  XMLParser ... ok !" << std::endl;
               
               FreeMemory(); // Pour supprimer la mémoire liée à l'allocation dynamique.
               
               return (true);
            }
                 
      }; // class XMLParser     
   }; // namespace XML

};// namespace XMLIOSERVER


#endif // __XMLIO_XML_PARSER__ 
