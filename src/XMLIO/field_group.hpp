#ifndef __FIELD_GROUP__
#define __FIELD_GROUP__


using XMLIOSERVER::XML::THashAttributes;

namespace XMLIOSERVER
{   
   class FieldGroup : public GroupTemplate<Field, FieldAttribut>
   {
      public:
      
         FieldGroup(void) : GroupTemplate<Field, FieldAttribut>()
         {/* Ne rien faire de plus */}               
         FieldGroup(const string& _id) : GroupTemplate<Field, FieldAttribut>(_id)
         {/* Ne rien faire de plus */}
         
         void setAttributes(const THashAttributes& attr)
         {
            for (THashAttributes::ConstIterator it = attr.begin(); it != attr.end(); it++)
               if ((*it).first.compare(string("id"))) // Non prise en compte de l'identifiant lors de l'affectation des attributs.
                  this->setSAttribut((*it).first, (*it).second);
            
            return;
         }
                 
         virtual const char* getName(void) const {return ("FieldGroup"); }  
         
         void parse (XMLNode& _node)
         {
            string name = _node.getElementName();            
            THashAttributes attributes;
            
            /// PARSING GESTION DES ATTRIBUTS ///
            _node.getAttributes(attributes);  
            this->setAttributes(attributes);
            attributes.clear();
               
            /// PARSING POUR GESION DES ENFANTS
            if (!(_node.goToChildElement()))
               WARNING("Le groupe de champ ne contient pas d'enfant !");
            else
            {
               //////////////////////////////////////
               do { // Parcours des contexts pour traitement. 
                            
                  string name = _node.getElementName();
                  attributes.clear();
                  _node.getAttributes(attributes);  
                                 
                  if (name.compare("field_group") == 0)
                  { // Parsing pour les groupes de champs
                  
                     FieldGroup* fgroup = NULL;
                        
                     if (attributes.end() != attributes.find("id"))
                     {// Si l'identifiant est défini.
                        if (FieldGroup::HasObject(attributes["id"]))
                           WARNING("Dans le context actuel, un groupe de champ du même nom existe déjà, le second fera référence au premier par défaut !"); // TODO TODO
                        fgroup =(FieldGroup*)&createGroup(attributes["id"]);
                        fgroup->parse(_node);
                     }
                     else
                     {// Si l'identifiant n'est pas défini.
                        fgroup = (FieldGroup*)&createGroup();
                        fgroup->parse(_node);
                     }
                        
                     continue;
                        
                  }
                  else if (name.compare("field") == 0)
                  { // Parsing pour les champs
                  
                     Field* field = NULL;
                  
                     if (attributes.end() != attributes.find("id"))
                     {// Si l'identifiant est défini.
                        if (Field::HasObject(attributes["id"]))
                           WARNING("Dans le context actuel, un champ du même nom existe déjà, le second fera référence au premier par défaut !");  // TODO TODO
                        field =(Field*)&createChild(attributes["id"]);
                        field->parse(_node);
                     }
                     else
                     {// Si l'identifiant n'est pas défini.
                        field = (Field*)&createChild();
                        field->parse(_node);
                     }         
                    
                     continue;
                  } 
                  else
                     WARNING("Un groupe de champs ne peut contenir qu'un champ ou un autre groupe de champs !");
                     
               } while (_node.goToNextElement());
               //////////////////////////////////////
               _node.goToParentElement(); // Retour au parent  
            }
                                       
            return;
         }
         
         virtual ~FieldGroup(void) 
         {/* Ne rien faire de plus */}   
         
   }; // class FieldGroup
     
   typedef FieldGroup FieldDefinition ;
   
}; // namespace XMLIOSERVER
   
#endif // __FIELD_GROUP__

