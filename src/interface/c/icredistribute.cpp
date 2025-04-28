/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */



 #include "xios.hpp"

 #include "object_template.hpp"
 #include "group_template.hpp"
 #include "attribute_template.hpp"
 
 #include "icutil.hpp"
 #include "timer.hpp"
 #include "redistribute_scalar.hpp"
 #include "redistribute_domain.hpp"
 #include "redistribute_axis.hpp"
 
 extern "C"
 {
 // /////////////////////////////// Définitions ////////////////////////////// //
 
    // ----------------------- Redéfinition de types ----------------------------
 
    typedef xios::CRedistributeDomain    * XRedistributeDomainPtr;
    typedef xios::CRedistributeAxis      * XRedistributeAxisPtr;
    typedef xios::CRedistributeScalar    * XRedistributeScalarPtr;
 
    // ------------------------ Création des handle -----------------------------
    void cxios_redistribute_domain_handle_create(XRedistributeDomainPtr * _ret, const char * _id, int _id_len)
    TRY
    {
       std::string id;
       if (!cstr2string(_id, _id_len, id)) return;
       CTimer::get("XIOS").resume() ;
       *_ret = xios::CRedistributeDomain::get(id);
       CTimer::get("XIOS").suspend() ;
    }
    CATCH_DUMP_STACK
 
    // -------------------- Vérification des identifiants -----------------------
    void cxios_redistribute_domain_valid_id(bool * _ret, const char * _id, int _id_len)
    TRY
    {
       std::string id;
       if (!cstr2string(_id, _id_len, id)) return;
 
       CTimer::get("XIOS").resume() ;
       *_ret = xios::CRedistributeDomain::has(id);
       CTimer::get("XIOS").suspend() ;
    }
    CATCH_DUMP_STACK
 
 // ------------------------ Création des handle -----------------------------
 void cxios_redistribute_axis_handle_create(XRedistributeAxisPtr * _ret, const char * _id, int _id_len)
 TRY
 {
    std::string id;
    if (!cstr2string(_id, _id_len, id)) return;
    CTimer::get("XIOS").resume() ;
    *_ret = xios::CRedistributeAxis::get(id);
    CTimer::get("XIOS").suspend() ;
 }
 CATCH_DUMP_STACK

 // -------------------- Vérification des identifiants -----------------------
 void cxios_redistribute_axis_valid_id(bool * _ret, const char * _id, int _id_len)
 TRY
 {
    std::string id;
    if (!cstr2string(_id, _id_len, id)) return;

    CTimer::get("XIOS").resume() ;
    *_ret = xios::CRedistributeAxis::has(id);
    CTimer::get("XIOS").suspend() ;
 }
 CATCH_DUMP_STACK
 
  // ------------------------ Création des handle -----------------------------
  void cxios_redistribute_scalar_handle_create(XRedistributeScalarPtr * _ret, const char * _id, int _id_len)
  TRY
  {
     std::string id;
     if (!cstr2string(_id, _id_len, id)) return;
     CTimer::get("XIOS").resume() ;
     *_ret = xios::CRedistributeScalar::get(id);
     CTimer::get("XIOS").suspend() ;
  }
  CATCH_DUMP_STACK

  // -------------------- Vérification des identifiants -----------------------
  void cxios_redistribute_scalar_valid_id(bool * _ret, const char * _id, int _id_len)
  TRY
  {
     std::string id;
     if (!cstr2string(_id, _id_len, id)) return;

     CTimer::get("XIOS").resume() ;
     *_ret = xios::CRedistributeScalar::has(id);
     CTimer::get("XIOS").suspend() ;
  }
  CATCH_DUMP_STACK
  
}// extern "C"
 