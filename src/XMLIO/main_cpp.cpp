#include "field_attribut.hpp"
#include "field.hpp"
#include "group_template.hpp"
#include "xmlio_std.hpp"


extern "C" void main_c_(void) ;

  
void main_c_(void)
{
  try 
  {
    group<Field>* FirstGroup=new group<Field>("1") ;
    group<Field> * MyGroup=FirstGroup->getNewGroup("group");
    Field* MyField=MyGroup->getNewChild("toto") ;
    MyField->setAttribut("name",string("titi")) ;
    MyField->setAttribut("level",10) ;
    
    MyField=MyGroup->getNewChild("tata") ;
    MyField->setAttribut("name",string("tutu")) ;
    MyField->setAttribut("level",25) ;

    MyField=MyGroup->getOrAppendChild("tete") ;
    MyField->setAttribut("level",1) ;
    
    MyField=MyGroup->getChild("toto") ;
    MyField->setAttribut("level",11) ;
    
    MyField->level=1 ;
    
    FirstGroup->Print() ;
    StdOut<<iendl;
    StdOut<<"This is the end"<<iendl ;
  }
  catch(ex_error x)
  {
    StdOut<<x.what<<endl ;
  }
}
