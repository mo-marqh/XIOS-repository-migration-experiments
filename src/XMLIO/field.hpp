#ifndef __FIELD__
#define __FIELD__

using XMLIOSERVER::XML::XMLNode;
using XMLIOSERVER::XML::THashAttributes;

namespace XMLIOSERVER
{
   class Field : public ObjectTemplate<Field>, public FieldAttribut
   {
      public:
         Field(void) : ObjectTemplate<Field>(), FieldAttribut()
			{/* Ne rien faire de plus */}	         	
			Field(const string& _id) : ObjectTemplate<Field>(_id), FieldAttribut()
         {/* Ne rien faire de plus */}	
         
         void setAttributes(const THashAttributes& attr)
         {
            for (THashAttributes::ConstIterator it = attr.begin(); it != attr.end(); it++)
               if ((*it).first.compare(string("id"))) // Non prise en compte de l'identifiant lors de l'affectation des attributs.
                  this->setSAttribut((*it).first, (*it).second);
            
            return;
         }
         
         virtual const char* getName(void) const {return ("Field"); }   

			void parse (XMLNode& _node)
			{
            string name = _node.getElementName();            
            THashAttributes attributes;
            
            /// PARSING GESTION DES ATTRIBUTS ///
            _node.getAttributes(attributes);  
            this->setAttributes(attributes);
            attributes.clear();
            
            /// PARSING POUR GESION DES ENFANTS
            // Rien à faire.
            
            return;
         }
         
         ~Field(void) 
         { /* Ne rien faire de plus */ }    
      
   }; // class Field 
      
}; // namespace XMLIOSERVER
   
#endif // __FIELD__
