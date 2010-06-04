#ifndef __CONTEXT__
#define __CONTEXT__ 

      
namespace XMLIOSERVER
{ 
   class Context : public ObjectTemplate<Context>
   {
      public:
            
         Context(void) : ObjectTemplate<Context>()
         {/* Ne rien faire de plus */}               
         Context(const string& _id) : ObjectTemplate<Context>(_id)
         {/* Ne rien faire de plus */}
         
         static void SetCurrentContext(const string& id)
         {
            // On modifie le context courrant pour tout les ObjectTemplate
            Context::SetContext(id);
            FieldGroup::SetContext(id);
            Field::SetContext(id);
         }
         
         void parse (XMLNode& _node)
         {
            THashAttributes attributes;
           
            /// PARSING POUR GESTION DES ENFANTS
            if (_node.getElementName().compare(string("context")))
               WARNING("Le noeud est mal nommé mais sera traité comme un context !");
            
            if (!(_node.goToChildElement()))
               WARNING("Le context ne contient pas d'enfant !"); 
            else
            {
               //////////////////////////////////////
              do { // Parcours des contexts pour traitement.        
                            
                  string name = _node.getElementName();
                  attributes.clear();
                  _node.getAttributes(attributes);   
                  
                  if (attributes.end() != attributes.find("id"))
                  { WARNING("Le noeud de définition possède un identifiant, ce dernier ne sera pas pris en compte lors du traitement !"); }
                  
                  if (name.compare("field_definition") == 0)
                  { // Parsing pour la définition des champs.
                  
                     if (FieldDefinition::HasObject("field_definition")) 
                        WARNING("Le context possède déjà un noeud de définition de champs, le dernier défini complétera le premier !");
                     
                     fieldDef = (FieldDefinition*)&FieldDefinition::CreateObject("field_definition"); // << Conversion possible car la classe Field n'a pas de propriétés.
                     fieldDef->parse(_node);

                     continue;
                  }
                  else if (name.compare("file_definition") == 0)
                  { // Parsing pour la définition des fichiers.                  
                     INFO("Le parsing des définitions de fichiers n'est pas encore implémenté");
                  } 
                  else if (name.compare("axis_definition") == 0)
                  { // Parsing pour la définition des axes.
                     INFO("Le parsing des définitions d'axes n'est pas encore implémenté");
                  }
                  else if (name.compare("grid_definition") == 0)
                  { // Parsing pour la définition des grilles.
                     INFO("Le parsing des définitions de grilles n'est pas encore implémenté");
                  } 
                  else
                     WARNING("La définition est invalide, seules les champs, grilles, axes et fichiers peuvent être définis !");
                     
                     
                  // Traitement file, grid et axis à compléter.
               } while (_node.goToNextElement());
               //////////////////////////////////////
               _node.goToParentElement(); // Retour au parent
            }
            
            return;
         }
         
         virtual const char* getName(void) const {return ("Context"); }
         
         FieldDefinition* getFieldDefinition(void) { return (this->fieldDef); }
                  
         ~Context()
         { delete fieldDef; }
         
      protected:
      
      private:
      
         FieldDefinition*  fieldDef;
         /*FileDefinition* fileDef;
         AxisDefinition*   axisDef;
         GridDefinition*   gridDef;*/

      
   }; //class Context
}// namespace XMLIOSERVER

#endif  // __CONTEXT__
