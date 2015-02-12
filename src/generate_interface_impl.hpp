#ifndef __XIOS_GENERATE_INTERFACE_IMPL_HPP__
#define __XIOS_GENERATE_INTERFACE_IMPL_HPP__

#include "xmlioserver_spl.hpp"
#include "generate_interface.hpp"
#include "type_util.hpp"
#include "indent.hpp"
#include "enum.hpp"
#include "array_new.hpp"
#include "date.hpp"

namespace xios
{
  template<> string CInterface::getStrFortranType<int>(void) {return string("INTEGER") ;}
  template<> string CInterface::getStrFortranType<bool>(void) {return string("LOGICAL") ;}
  template<> string CInterface::getStrFortranType<double>(void) {return string("REAL") ;}
  template<> string CInterface::getStrFortranType<float>(void) {return string("REAL") ;}
  template<> string CInterface::getStrFortranType<CDate>(void) {return string("TYPE(txios(date))") ;}
  template<> string CInterface::getStrFortranType<CDuration>(void) {return string("TYPE(txios(duration))") ;}

  template<> string CInterface::getStrFortranKind<int>(void) {return string("") ;}
  template<> string CInterface::getStrFortranKind<bool>(void) {return string("") ;}
  template<> string CInterface::getStrFortranKind<double>(void) {return string("(KIND=8)") ;}
  template<> string CInterface::getStrFortranKind<float>(void) {return string("(KIND=4)") ;}
  template<> string CInterface::getStrFortranKind<CDate>(void) {return string("") ;}
  template<> string CInterface::getStrFortranKind<CDuration>(void) {return string("") ;}

  template<> string CInterface::getStrFortranKindC<int>(void) {return string("(KIND=C_INT)") ;}
  template<> string CInterface::getStrFortranKindC<bool>(void) {return string("(KIND=C_BOOL)") ;}
  template<> string CInterface::getStrFortranKindC<double>(void) {return string("(KIND=C_DOUBLE)") ;}
  template<> string CInterface::getStrFortranKindC<float>(void) {return string("(KIND=C_FLOAT)") ;}
  template<> string CInterface::getStrFortranKindC<CDate>(void) {return string("") ;}
  template<> string CInterface::getStrFortranKindC<CDuration>(void) {return string("") ;}

  template<> bool CInterface::matchingTypeCFortran<int>(void) { return true ; }
  template<> bool CInterface::matchingTypeCFortran<bool>(void) { return false ;}
  template<> bool CInterface::matchingTypeCFortran<double>(void) { return true; }
  template<> bool CInterface::matchingTypeCFortran<float>(void) { return true; }
  template<> bool CInterface::matchingTypeCFortran<CDate>(void) { return true; }
  template<> bool CInterface::matchingTypeCFortran<CDuration>(void) { return true; }


// /////////////////////////////////////////////////
// //                 C Interface                 //
// /////////////////////////////////////////////////


  void CInterface::AttributeIsDefinedCInterface(ostream& oss, const string& className,const string& name)
  {
    oss<<"bool cxios_is_defined_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl )"<<iendl ;
    oss<<"{"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ;
    oss<<"  return "<<className<<"_hdl->"<<name<<".hasInheritedValue();"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;
    oss<<"}"<<iendl ;
    oss<<iendl ;
  }

  template <class T>
  void CInterface::AttributeCInterface(ostream& oss, const string& className,const string& name)
  {
    string typeName=getStrType<T>() ;

    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<" "<<name<<")"<<iendl ;
    oss<<"{"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ;
    oss<<"  "<<className<<"_hdl->"<<name<<".setValue("<<name<<");"<<iendl ;
//    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;
    oss<<"}"<<iendl ;

    oss<<iendl ;
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<")"<<iendl ;
    oss<<"{"<<iendl;
    oss<<"  *"<<name<<" = "<<className<<"_hdl->"<<name<<".getInheritedValue();"<<iendl ;
    oss<<"}"<<iendl ;
    oss<<iendl ;
  }


  template<>
  void CInterface::AttributeCInterface<string>(ostream& oss, const string& className,const string& name)
  {
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, const char * "<<name<<", int "<<name<<"_size)"<<iendl ;
    oss<<"{"<<iendl ;
    oss<<"  std::string "<<name<<"_str;"<<iendl;
    oss<<"  if(!cstr2string("<<name<<", "<<name<<"_size, "<<name<<"_str)) return;"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ;
    oss<<"  "<<className<<"_hdl->"<<name<<".setValue("<<name<<"_str);"<<iendl ;
//    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;
    oss<<"}"<<iendl ;

    oss<<iendl ;

    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, char * "<<name<<", int "<<name<<"_size)"<<iendl ;
    oss<<"{"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ;
    oss<<"  if(!string_copy("<<className<<"_hdl->"<<name<<".getInheritedValue(),"<<name<<" , "<<name<<"_size))"<<iendl ;
    oss<<"    ERROR(\"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, char * "<<name<<", int "
       <<name<<"_size)\", <<\"Input string is to short\");"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;
    oss<<"}"<<iendl ;
    oss<<iendl ;

  }

  template<>
  void CInterface::AttributeCInterface<CEnumBase>(ostream& oss, const string& className,const string& name)
  {
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, const char * "<<name<<", int "<<name<<"_size)"<<iendl ;
    oss<<"{"<<iendl ;
    oss<<"  std::string "<<name<<"_str;"<<iendl;
    oss<<"  if(!cstr2string("<<name<<", "<<name<<"_size, "<<name<<"_str)) return;"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ;
    oss<<"  "<<className<<"_hdl->"<<name<<".fromString("<<name<<"_str);"<<iendl ;
//    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;
    oss<<"}"<<iendl ;

    oss<<iendl ;

    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, char * "<<name<<", int "<<name<<"_size)"<<iendl ;
    oss<<"{"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ;
    oss<<"  if(!string_copy("<<className<<"_hdl->"<<name<<".getInheritedStringValue(),"<<name<<" , "<<name<<"_size))"<<iendl ;
    oss<<"    ERROR(\"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, char * "<<name<<", int "
       <<name<<"_size)\", <<\"Input string is to short\");"<<iendl ;
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;
    oss<<"}"<<iendl ;
    oss<<iendl ;

  }
//     if (!array_copy(domain_hdl->mask.getValue(), mask, extent1, extent2))
//        ERROR("cxios_get_domain_mask(XDomainPtr domain_hdl, bool * mask, int extent1, int extent2)",<<"Output array size is not conform to array size attribut") ;

  template<>
  void CInterface::AttributeCInterface<CDate>(ostream& oss, const string& className,const string& name)
  {
    oss << "void cxios_set_" << className << "_" << name << "(" << className << "_Ptr " << className << "_hdl, cxios_date " << name << "_c)" << iendl;
    oss << "{" << iendl;
    oss << "  CTimer::get(\"XIOS\").resume();" << iendl;
    oss << "  " << className << "_hdl->" << name << ".allocate();" << iendl;
    oss << "  CDate& " << name <<" = " << className << "_hdl->" << name << ".get();" << iendl;
    oss << "  " << name << ".setDate(" << name << "_c.year," << iendl;
    oss << "                         " << name << "_c.month," << iendl;
    oss << "                         " << name << "_c.day," << iendl;
    oss << "                         " << name << "_c.hour," << iendl;
    oss << "                         " << name << "_c.minute," << iendl;
    oss << "                         " << name << "_c.second);" << iendl;
    oss << "  if (" << name << ".hasRelCalendar())" << iendl;
    oss << "    " << name << ".checkDate();" << iendl;
    oss << "  CTimer::get(\"XIOS\").suspend();" << iendl;
    oss << "}" << iendl;

    oss << iendl;

    oss << "void cxios_get_" << className << "_" << name << "(" << className << "_Ptr " << className << "_hdl, cxios_date* " << name << "_c)" << iendl;
    oss << "{" << iendl;
    oss << "  CTimer::get(\"XIOS\").resume();" << iendl;
    oss << "  CDate " << name <<" = " << className << "_hdl->" << name << ".getInheritedValue();" << iendl;
    oss << "  " << name << "_c->year = " << name << ".getYear();" << iendl;
    oss << "  " << name << "_c->month = " << name << ".getMonth();" << iendl;
    oss << "  " << name << "_c->day = " << name << ".getDay();" << iendl;
    oss << "  " << name << "_c->hour = " << name << ".getHour();" << iendl;
    oss << "  " << name << "_c->minute = " << name << ".getMinute();" << iendl;
    oss << "  " << name << "_c->second = " << name << ".getSecond();" << iendl;
    oss << "  CTimer::get(\"XIOS\").suspend();" << iendl;
    oss << "}" << iendl;
    oss << iendl;
  }

  template<>
  void CInterface::AttributeCInterface<CDuration>(ostream& oss, const string& className,const string& name)
  {
    oss << "void cxios_set_" << className << "_" << name << "(" << className << "_Ptr " << className << "_hdl, cxios_duration " << name << "_c)" << iendl;
    oss << "{" << iendl;
    oss << "  CTimer::get(\"XIOS\").resume();" << iendl;
    oss << "  " << className << "_hdl->" << name << ".allocate();" << iendl;
    oss << "  CDuration& " << name <<" = " << className << "_hdl->" << name << ".get();" << iendl;
    oss << "  " << name << ".year = " << name << "_c.year;" << iendl;
    oss << "  " << name << ".month = " << name << "_c.month;" << iendl;
    oss << "  " << name << ".day = " << name << "_c.day;" << iendl;
    oss << "  " << name << ".hour = " << name << "_c.hour;" << iendl;
    oss << "  " << name << ".minute = " << name << "_c.minute;" << iendl;
    oss << "  " << name << ".second = " << name << "_c.second;" << iendl;
    oss << "  " << name << ".timestep = " << name << "_c.timestep;" << iendl;
    oss << "  CTimer::get(\"XIOS\").suspend();" << iendl;
    oss << "}" << iendl;

    oss << iendl;

    oss << "void cxios_get_" << className << "_" << name << "(" << className << "_Ptr " << className << "_hdl, cxios_duration* " << name << "_c)" << iendl;
    oss << "{" << iendl;
    oss << "  CTimer::get(\"XIOS\").resume();" << iendl;
    oss << "  CDuration " << name <<" = " << className << "_hdl->" << name << ".getInheritedValue();" << iendl;
    oss << "  " << name << "_c->year = " << name << ".year;" << iendl;
    oss << "  " << name << "_c->month = " << name << ".month;" << iendl;
    oss << "  " << name << "_c->day = " << name << ".day;" << iendl;
    oss << "  " << name << "_c->hour = " << name << ".hour;" << iendl;
    oss << "  " << name << "_c->minute = " << name << ".minute;" << iendl;
    oss << "  " << name << "_c->second = " << name << ".second;" << iendl;
    oss << "  " << name << "_c->timestep = " << name << ".timestep;" << iendl;
    oss << "  CTimer::get(\"XIOS\").suspend();" << iendl;
    oss << "}" << iendl;
    oss << iendl;
  }

/*
#define macro(T) \
  template <>\
  void CInterface::AttributeCInterface<ARRAY(T,1)>(ostream& oss, const string& className,const string& name)\
  {\
    string typeName=getStrType<T>() ;\
\
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1)"<<iendl ;\
    oss<<"{"<<iendl ;\
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  ARRAY("<<typeName<<",1) array_tmp(new CArray<"<<typeName<<",1>(boost::extents[extent1]));"<<iendl ;\
    oss<<"  std::copy("<<name<<", &("<<name<<"[array_tmp->num_elements()]), array_tmp->data());"<<iendl ;\
    oss<<"  "<<className<<"_hdl->"<<name<<".setValue(array_tmp);"<<iendl ;\
//    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl; \
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1)"<<iendl ;\
    oss<<"{"<<iendl; \
    oss<<"  if (!array_copy("<<className<<"_hdl->"<<name<<".getValue(), "<<name<<", extent1))"<<iendl ; \
    oss<<"   ERROR(\"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1)\",<<" \
       <<"\"Output array size is not conform to array size attribute\") ;"<<iendl; \
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
  }\
\
  template <> \
  void CInterface::AttributeCInterface<ARRAY(T,2)>(ostream& oss, const string& className,const string& name)\
  {\
    string typeName=getStrType<T>() ;\
\
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2)"<<iendl ;\
    oss<<"{"<<iendl ;\
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  ARRAY("<<typeName<<",2) array_tmp(new CArray<"<<typeName<<",2>(boost::extents[extent1][extent2]));"<<iendl ;\
    oss<<"  std::copy("<<name<<", &("<<name<<"[array_tmp->num_elements()]), array_tmp->data());"<<iendl ;\
    oss<<"  "<<className<<"_hdl->"<<name<<".setValue(array_tmp);"<<iendl ;\
//    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl; \
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2)"<<iendl ;\
    oss<<"{"<<iendl; \
    oss<<"  if (!array_copy("<<className<<"_hdl->"<<name<<".getValue(), "<<name<<", extent1, extent2))"<<iendl ; \
    oss<<"   ERROR(\"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2)\",<<" \
       <<"\"Output array size is not conform to array size attribute\") ;"<<iendl; \
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
  }\
\
  template <>\
  void CInterface::AttributeCInterface<ARRAY(T,3)>(ostream& oss, const string& className,const string& name)\
  {\
    string typeName=getStrType<T>() ;\
\
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2, int extent3)"<<iendl ;\
    oss<<"{"<<iendl ;\
    oss<<"   CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  ARRAY("<<typeName<<",3) array_tmp(new CArray<"<<typeName<<",3>(boost::extents[extent1][extent2][extent3]));"<<iendl ;\
    oss<<"  std::copy("<<name<<", &("<<name<<"[array_tmp->num_elements()]), array_tmp->data());"<<iendl ;\
    oss<<"  "<<className<<"_hdl->"<<name<<".setValue(array_tmp);"<<iendl ;\
//    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl; \
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2, int extent3)"<<iendl ;\
    oss<<"{"<<iendl; \
    oss<<"  if (!array_copy("<<className<<"_hdl->"<<name<<".getValue(), "<<name<<", extent1))"<<iendl ; \
    oss<<"   ERROR(\"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2, int extent3)\",<<" \
       <<"\"Output array size is not conform to array size attribute\") ;"<<iendl; \
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
  }

macro(bool)
macro(double)
macro(int)
*/

#undef macro

// /////////////////////////////////////////////////
// //          Fortran 2003 Interface             //
// /////////////////////////////////////////////////
   void CInterface::AttributeIsDefinedFortran2003Interface(ostream& oss,const string& className,const string& name)
   {
     oss<<"FUNCTION cxios_is_defined_"<<className<<"_"<<name<<"("<<className<<"_hdl ) BIND(C)"<<iendl ;
     oss<<"  USE ISO_C_BINDING"<<iendl ;
     oss<<"  LOGICAL(kind=C_BOOL) :: cxios_is_defined_"<<className<<"_"<<name<<iendl;
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE :: "<<className<<"_hdl"<<iendl ;
     oss<<"END FUNCTION cxios_is_defined_"<<className<<"_"<<name<<iendl ;
   }

   template <class T>
   void CInterface::AttributeFortran2003Interface(ostream& oss,const string& className,const string& name)
   {
     string fortranType=getStrFortranType<T>() ;
     string fortranKindC=getStrFortranKindC<T>() ;

     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<") BIND(C)"<<iendl ;
     oss<<"  USE ISO_C_BINDING"<<iendl ;
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE :: "<<className<<"_hdl"<<iendl ;
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"      , VALUE :: "<<name<<iendl ;
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ;
     oss<<iendl ;
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<") BIND(C)"<<iendl ;
     oss<<"  USE ISO_C_BINDING"<<iendl ;
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE :: "<<className<<"_hdl"<<iendl ;
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"             :: "<<name<<iendl ;
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ;
     oss<<iendl ;
   }


   template <>
   void CInterface::AttributeFortran2003Interface<string>(ostream& oss,const string& className,const string& name)
   {

     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", "<<name<<"_size) BIND(C)"<<iendl ;
     oss<<"  USE ISO_C_BINDING"<<iendl ;
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE :: "<<className<<"_hdl"<<iendl ;
     oss<<"  CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: "<<name<<iendl ;
     oss<<"  INTEGER  (kind = C_INT)     , VALUE        :: "<<name<<"_size"<<iendl ;
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ;
     oss<<iendl ;
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", "<<name<<"_size) BIND(C)"<<iendl ;
     oss<<"  USE ISO_C_BINDING"<<iendl ;
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE :: "<<className<<"_hdl"<<iendl ;
     oss<<"  CHARACTER(kind = C_CHAR)    , DIMENSION(*) :: "<<name<<iendl ;
     oss<<"  INTEGER  (kind = C_INT)     , VALUE        :: "<<name<<"_size"<<iendl ;
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ;
     oss<<iendl ;
   }

  template <>
  void CInterface::AttributeFortran2003Interface<CDate>(ostream& oss, const string& className, const string& name)
  {
    oss << "SUBROUTINE cxios_set_" << className << "_" << name << "(" << className << "_hdl, " << name << ") BIND(C)" << iendl;
    oss << "  USE ISO_C_BINDING" << iendl;
    oss << "  USE IDATE" << iendl;
    oss << "  INTEGER (kind = C_INTPTR_T), VALUE :: " << className << "_hdl" << iendl;
    oss << "  TYPE(txios(date)), VALUE :: " << name << iendl;
    oss << "END SUBROUTINE cxios_set_" << className << "_" << name << iendl;
    oss << iendl;
    oss << "SUBROUTINE cxios_get_" << className << "_" << name << "(" << className << "_hdl, " << name << ") BIND(C)" << iendl;
    oss << "  USE ISO_C_BINDING" << iendl;
    oss << "  USE IDATE" << iendl;
    oss << "  INTEGER (kind = C_INTPTR_T), VALUE :: " << className << "_hdl" << iendl;
    oss << "  TYPE(txios(date)) :: " << name << iendl;
    oss << "END SUBROUTINE cxios_get_" << className << "_" << name << iendl;
    oss << iendl;
  }

  template <>
  void CInterface::AttributeFortran2003Interface<CDuration>(ostream& oss, const string& className, const string& name)
  {
    oss << "SUBROUTINE cxios_set_" << className << "_" << name << "(" << className << "_hdl, " << name << ") BIND(C)" << iendl;
    oss << "  USE ISO_C_BINDING" << iendl;
    oss << "  USE IDURATION" << iendl;
    oss << "  INTEGER (kind = C_INTPTR_T), VALUE :: " << className << "_hdl" << iendl;
    oss << "  TYPE(txios(duration)), VALUE :: " << name << iendl;
    oss << "END SUBROUTINE cxios_set_" << className << "_" << name << iendl;
    oss << iendl;
    oss << "SUBROUTINE cxios_get_" << className << "_" << name << "(" << className << "_hdl, " << name << ") BIND(C)" << iendl;
    oss << "  USE ISO_C_BINDING" << iendl;
    oss << "  USE IDURATION" << iendl;
    oss << "  INTEGER (kind = C_INTPTR_T), VALUE :: " << className << "_hdl" << iendl;
    oss << "  TYPE(txios(duration)) :: " << name << iendl;
    oss << "END SUBROUTINE cxios_get_" << className << "_" << name << iendl;
    oss << iendl;
  }

/*
#define macro(T)\
   template <>\
   void CInterface::AttributeFortran2003Interface<ARRAY(T,1)>(ostream& oss,const string& className,const string& name) \
   { \
     string fortranType=getStrFortranType<T>() ; \
     string fortranKindC=getStrFortranKindC<T>() ; \
      \
     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl; \
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortran2003Interface<ARRAY(T,2)>(ostream& oss,const string& className,const string& name) \
   { \
     string fortranType=getStrFortranType<T>() ; \
     string fortranKindC=getStrFortranKindC<T>() ; \
      \
     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ; \
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ; \
   } \
     \
   template <> \
   void CInterface::AttributeFortran2003Interface<ARRAY(T,3)>(ostream& oss,const string& className,const string& name) \
   { \
     string fortranType=getStrFortranType<T>() ; \
     string fortranKindC=getStrFortranKindC<T>() ; \
      \
     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2, extent3) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent3"<<iendl ; \
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ;\
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2, extent3) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent3"<<iendl ; \
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

  #undef macro
*/
   template <class T>
   void CInterface::AttributeFortranInterfaceDeclaration(ostream& oss,const string& className,const string& name)
   {
     oss<<getStrFortranType<T>()<<" "<< getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<iendl ;
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>()<<" :: "<<name<<"_tmp"<<iendl ;
   }

   template <class T>
   void CInterface::AttributeFortranInterfaceGetDeclaration(ostream& oss,const string& className,const string& name)
   {
     oss<<getStrFortranType<T>()<<" "<< getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<iendl ;
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>()<<" :: "<<name<<"_tmp"<<iendl ;
   }

   void CInterface::AttributeFortranInterfaceIsDefinedDeclaration(ostream& oss,const string& className,const string& name)
   {
     oss<<"LOGICAL, OPTIONAL, INTENT(OUT) :: "<<name<<iendl ;
     oss<<"LOGICAL(KIND=C_BOOL) :: "<<name<<"_tmp"<<iendl ;
   }

   template <>
   void CInterface::AttributeFortranInterfaceDeclaration<string>(ostream& oss,const string& className,const string& name)
   {
     oss<<"CHARACTER(len = *) , OPTIONAL, INTENT(IN) :: "<<name<<iendl ;
   }

   template <>
   void CInterface::AttributeFortranInterfaceGetDeclaration<string>(ostream& oss,const string& className,const string& name)
   {
     oss<<"CHARACTER(len = *) , OPTIONAL, INTENT(OUT) :: "<<name<<iendl ;
   }

/*
#define macro(T)\
   template <> \
   void CInterface::AttributeFortranInterfaceDeclaration<ARRAY(T,1)>(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<"(:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:)"<<iendl ; \
   } \
   template <> \
   void CInterface::AttributeFortranInterfaceGetDeclaration<ARRAY(T,1)>(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<"(:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:)"<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortranInterfaceDeclaration<ARRAY(T,2)>(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<"(:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:)"<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortranInterfaceGetDeclaration<ARRAY(T,2)>(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<"(:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:)"<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortranInterfaceDeclaration<ARRAY(T,3)>(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<"(:,:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:,:)"<<iendl ; \
   }\
 \
   template <> \
   void CInterface::AttributeFortranInterfaceGetDeclaration<ARRAY(T,3)>(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<"(:,:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:,:)"<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

#undef macro
*/

   template <class T>
   void CInterface::AttributeFortranInterfaceBody(ostream& oss,const string& className,const string& name)
   {
     string name_tmp=name+"__tmp" ;

     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ;
     if (!matchingTypeCFortran<T>())
     {
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ;
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<")"<<iendl ;
     }
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_)"<<iendl ;
     oss<<"ENDIF"<<iendl ;
   }

   template <class T>
   void CInterface::AttributeFortranInterfaceGetBody(ostream& oss,const string& className,const string& name)
   {
     string name_tmp=name+"__tmp" ;

     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ;
     if (!matchingTypeCFortran<T>())
     {
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<")"<<iendl ;
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ;
     }
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_)"<<iendl ;
     oss<<"ENDIF"<<iendl ;
   }

   void CInterface::AttributeFortranInterfaceIsDefinedBody(ostream& oss,const string& className,const string& name)
   {
     string name_tmp=name+"__tmp" ;

     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ;
     oss<<"  "<<name<<"__tmp=cxios_is_defined_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr)"<<iendl ;
     oss<<"  "<<name<<"_="<<name_tmp<<iendl ;
     oss<<"ENDIF"<<iendl ;
   }

   template <>
   void CInterface::AttributeFortranInterfaceBody<string>(ostream& oss,const string& className,const string& name)
   {
      oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ;
      oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_, len("<<name<<"_))"<<iendl ;
      oss<<"ENDIF"<<iendl ;
   }

   template <>
   void CInterface::AttributeFortranInterfaceGetBody<string>(ostream& oss,const string& className,const string& name)
   {
      oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ;
      oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_, len("<<name<<"_))"<<iendl ;
      oss<<"ENDIF"<<iendl ;
   }

/*
#define macro(T) \
   template <>  \
   void CInterface::AttributeFortranInterfaceBody< ARRAY(T,1) >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1)))"<<iendl ; \
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ; \
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1))"<<iendl ; \
     } \
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
 \
   template <>  \
   void CInterface::AttributeFortranInterfaceBody< ARRAY(T,2) >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2)))"<<iendl ; \
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ; \
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
     } \
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
    \
   template <>  \
   void CInterface::AttributeFortranInterfaceBody< ARRAY(T,3) >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3)))"<<iendl ; \
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ; \
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
     } \
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

#undef macro
*/

/*
#define macro(T) \
   template <>  \
   void CInterface::AttributeFortranInterfaceGetBody< ARRAY(T,1) >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1)))"<<iendl ; \
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1))"<<iendl ; \
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ; \
     } \
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
 \
   template <>  \
   void CInterface::AttributeFortranInterfaceGetBody< ARRAY(T,2) >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2)))"<<iendl ; \
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ; \
     } \
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
    \
   template <>  \
   void CInterface::AttributeFortranInterfaceGetBody< ARRAY(T,3) >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3)))"<<iendl ; \
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ; \
      } \
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

#undef macro
*/

// declaration for CArray




#define macro(T) \
  template <>\
  void CInterface::AttributeCInterface<CArray<T,1> >(ostream& oss, const string& className,const string& name)\
  {\
    string typeName=getStrType<T>() ;\
\
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1)"<<iendl ;\
    oss<<"{"<<iendl ;\
    oss<<"  CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  CArray<"<<typeName<<",1> tmp("<<name<<",shape(extent1),neverDeleteData) ;"<<iendl ;\
    oss<<"  "<<className<<"_hdl->"<<name<<".reference(tmp.copy());"<<iendl ;\
/*    oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;*/\
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl; \
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1)"<<iendl ;\
    oss<<"{"<<iendl; \
    oss<<"  CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  CArray<"<<typeName<<",1> tmp("<<name<<",shape(extent1),neverDeleteData) ;"<<iendl ;\
    oss<<"  tmp="<<className<<"_hdl->"<<name<<".getInheritedValue() ;"<<iendl ;\
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl ;\
  }\
\
  template <> \
  void CInterface::AttributeCInterface<CArray<T,2> >(ostream& oss, const string& className,const string& name)\
  {\
    string typeName=getStrType<T>() ;\
\
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2)"<<iendl ;\
    oss<<"{"<<iendl ;\
    oss<<"  CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  CArray<"<<typeName<<",2> tmp("<<name<<",shape(extent1,extent2),neverDeleteData) ;"<<iendl ;\
    oss<<"  "<<className<<"_hdl->"<<name<<".reference(tmp.copy());"<<iendl ;\
    /*oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;*/\
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl; \
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2)"<<iendl ;\
    oss<<"{"<<iendl; \
    oss<<"  CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  CArray<"<<typeName<<",2> tmp("<<name<<",shape(extent1,extent2),neverDeleteData) ;"<<iendl ;\
    oss<<"  tmp="<<className<<"_hdl->"<<name<<".getInheritedValue() ;"<<iendl ;\
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl ;\
  }\
\
  template <>\
  void CInterface::AttributeCInterface<CArray<T,3> >(ostream& oss, const string& className,const string& name)\
  {\
    string typeName=getStrType<T>() ;\
\
    oss<<"void cxios_set_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2, int extent3)"<<iendl ;\
    oss<<"{"<<iendl ;\
    oss<<"  CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  CArray<"<<typeName<<",3> tmp("<<name<<",shape(extent1,extent2,extent3),neverDeleteData) ;"<<iendl ;\
    oss<<"  "<<className<<"_hdl->"<<name<<".reference(tmp.copy());"<<iendl ;\
    /*oss<<"  "<<className<<"_hdl->sendAttributToServer("<<className<<"_hdl->"<<name<<");"<<iendl ;*/\
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl; \
    oss<<"void cxios_get_"<<className<<"_"<<name<<"("<<className<<"_Ptr "<<className<<"_hdl, "<< typeName<<"* "<<name<<", int extent1, int extent2, int extent3)"<<iendl ;\
    oss<<"{"<<iendl; \
    oss<<"  CTimer::get(\"XIOS\").resume();"<<iendl ; \
    oss<<"  CArray<"<<typeName<<",3> tmp("<<name<<",shape(extent1,extent2,extent3),neverDeleteData) ;"<<iendl ;\
    oss<<"  tmp="<<className<<"_hdl->"<<name<<".getInheritedValue() ;"<<iendl ;\
    oss<<"   CTimer::get(\"XIOS\").suspend();"<<iendl ;\
    oss<<"}"<<iendl ;\
    oss<<iendl ;\
  }

macro(bool)
macro(double)
macro(int)

#undef macro

// /////////////////////////////////////////////////
// //          Fortran 2003 Interface             //
// /////////////////////////////////////////////////



#define macro(T)\
   template <>\
   void CInterface::AttributeFortran2003Interface<CArray<T,1> >(ostream& oss,const string& className,const string& name) \
   { \
     string fortranType=getStrFortranType<T>() ; \
     string fortranKindC=getStrFortranKindC<T>() ; \
      \
     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl; \
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ;\
   } \
 \
   template <> \
   void CInterface::AttributeFortran2003Interface<CArray<T,2> >(ostream& oss,const string& className,const string& name) \
   { \
     string fortranType=getStrFortranType<T>() ; \
     string fortranKindC=getStrFortranKindC<T>() ; \
      \
     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ; \
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ;\
   } \
     \
   template <> \
   void CInterface::AttributeFortran2003Interface<CArray<T,3> >(ostream& oss,const string& className,const string& name) \
   { \
     string fortranType=getStrFortranType<T>() ; \
     string fortranKindC=getStrFortranKindC<T>() ; \
      \
     oss<<"SUBROUTINE cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2, extent3) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent3"<<iendl ; \
     oss<<"END SUBROUTINE cxios_set_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ;\
     oss<<"SUBROUTINE cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl, "<<name<<", extent1, extent2, extent3) BIND(C)"<<iendl ; \
     oss<<"  USE ISO_C_BINDING"<<iendl ; \
     oss<<"  INTEGER (kind = C_INTPTR_T), VALUE       :: "<<className<<"_hdl"<<iendl ; \
     oss<<"  "<<fortranType<<" "<<fortranKindC<<"     , DIMENSION(*) :: "<<name<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent1"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent2"<<iendl ; \
     oss<<"  INTEGER (kind = C_INT), VALUE  :: extent3"<<iendl ; \
     oss<<"END SUBROUTINE cxios_get_"<<className<<"_"<<name<<iendl ; \
     oss<<iendl ;\
   }

  macro(bool)
  macro(double)
  macro(int)

  #undef macro


#define macro(T)\
   template <> \
   void CInterface::AttributeFortranInterfaceDeclaration<CArray<T,1> >(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<"(:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:)"<<iendl ; \
   } \
   template <> \
   void CInterface::AttributeFortranInterfaceGetDeclaration<CArray<T,1> >(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<"(:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:)"<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortranInterfaceDeclaration<CArray<T,2> >(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<"(:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:)"<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortranInterfaceGetDeclaration<CArray<T,2> >(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<"(:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:)"<<iendl ; \
   } \
 \
   template <> \
   void CInterface::AttributeFortranInterfaceDeclaration<CArray<T,3> >(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(IN) :: "<<name<<"(:,:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:,:)"<<iendl ; \
   }\
 \
   template <> \
   void CInterface::AttributeFortranInterfaceGetDeclaration<CArray<T,3> >(ostream& oss,const string& className,const string& name) \
   { \
     oss<<getStrFortranType<T>()<<" "<<getStrFortranKind<T>() <<" , OPTIONAL, INTENT(OUT) :: "<<name<<"(:,:,:)"<<iendl ; \
     if (!matchingTypeCFortran<T>()) oss<<getStrFortranType<T>()<<" "<<getStrFortranKindC<T>() <<" , ALLOCATABLE :: "<<name<<"_tmp(:,:,:)"<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

#undef macro



#define macro(T) \
   template <>  \
   void CInterface::AttributeFortranInterfaceBody< CArray<T,1> >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1)))"<<iendl ; \
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ; \
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1))"<<iendl ; \
     } \
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
 \
   template <>  \
   void CInterface::AttributeFortranInterfaceBody< CArray<T,2> >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2)))"<<iendl ; \
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ; \
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
     } \
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
    \
   template <>  \
   void CInterface::AttributeFortranInterfaceBody< CArray<T,3> >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3)))"<<iendl ; \
       oss<<"  "<<name_tmp<<"="<<name<<"_"<<iendl ; \
       oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
     } \
     else oss<<"  CALL cxios_set_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

#undef macro

#define macro(T) \
   template <>  \
   void CInterface::AttributeFortranInterfaceGetBody< CArray<T,1> >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1)))"<<iendl ; \
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1))"<<iendl ; \
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ; \
     } \
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
 \
   template <>  \
   void CInterface::AttributeFortranInterfaceGetBody< CArray<T,2> >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2)))"<<iendl ; \
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ; \
     } \
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   } \
    \
   template <>  \
   void CInterface::AttributeFortranInterfaceGetBody< CArray<T,3> >(ostream& oss,const string& className,const string& name) \
   {  \
     string name_tmp=name+"__tmp" ; \
      \
     oss<<"IF (PRESENT("<<name<<"_)) THEN"<<iendl ; \
     if (!matchingTypeCFortran<T>())  \
     { \
       oss<<"  ALLOCATE("<<name_tmp<<"(size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3)))"<<iendl ; \
       oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name_tmp<<",size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
       oss<<"  "<<name<<"_="<<name_tmp<<iendl ; \
      } \
     else oss<<"  CALL cxios_get_"<<className<<"_"<<name<<"("<<className<<"_hdl%daddr, "<<name<<"_,size("<<name<<"_,1),size("<<name<<"_,2),size("<<name<<"_,3))"<<iendl ; \
     oss<<"ENDIF"<<iendl ; \
   }

  macro(bool)
  macro(double)
  macro(int)

#undef macro



































}

#endif
