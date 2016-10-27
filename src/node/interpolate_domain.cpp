#include "interpolate_domain.hpp"
#include "type.hpp"

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CInterpolateDomain::CInterpolateDomain(void)
    : CObjectTemplate<CInterpolateDomain>(), CInterpolateDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CInterpolateDomain::CInterpolateDomain(const StdString & id)
    : CObjectTemplate<CInterpolateDomain>(id), CInterpolateDomainAttributes(), CTransformation<CDomain>()
  { /* Ne rien faire de plus */ }

  CInterpolateDomain::~CInterpolateDomain(void)
  {}

  CTransformation<CDomain>* CInterpolateDomain::create(const StdString& id, xml::CXMLNode* node)
  {
    CInterpolateDomain* interpDomain = CInterpolateDomainGroup::get("interpolate_domain_definition")->createChild(id);
    if (node) interpDomain->parse(*node);
    return static_cast<CTransformation<CDomain>*>(interpDomain);
  }

  bool CInterpolateDomain::_dummyRegistered = CInterpolateDomain::registerTrans();
  bool CInterpolateDomain::registerTrans()
  {
    registerTransformation(TRANS_INTERPOLATE_DOMAIN, create);
  }

  //----------------------------------------------------------------

  StdString CInterpolateDomain::GetName(void)    { return StdString("interpolate_domain"); }
  StdString CInterpolateDomain::GetDefName(void) { return StdString("interpolate_domain"); }
  ENodeType CInterpolateDomain::GetType(void)    { return eInterpolateDomain; }

  void CInterpolateDomain::checkValid(CDomain* domainSrc)
  {
    int order = 2;
    if (!this->order.isEmpty()) order = this->order.getValue();
    else this->order.setValue(order);
    if (order < 1)
    {
       ERROR("void CInterpolateDomain::checkValid(CDomain* domainSrc)",
             << "Interpolation order is less than 1, it should be greater than 0."
             << "Please define a correct one") ;
    }

    StdString weightFile = "interpolation_weights_" + domainSrc->getDomainOutputName();

    if (!this->mode.isEmpty())
    { 
      if (mode_attr::read == this->mode)
      {
         if (this->file.isEmpty())
         {
            ERROR("void CInterpolateDomain::checkValid(CDomain* domainSrc)",
                 << "Read mode is activated but there is no file specified." << std::endl
                 << "Please define a correct file containing interpolation weights with option 'file'. ");
         }
         else
         {
           weightFile = this->file;
           ifstream f(weightFile.c_str());
           if (!f.good())
             ERROR("void CInterpolateDomain::checkValid(CDomain* domainSrc)",
                   << "Read mode is activated but file "  << weightFile << " doesn't exist." << std::endl
                   << "Please check this file ");
         }
      }
      else
      {
        if (file.isEmpty())
          this->file.setValue(weightFile);
      }   
    }
    else
    {
      if (!file.isEmpty()) // set mode read
      {
         weightFile = this->file;
         ifstream f(weightFile.c_str());
         if (!f.good()) // file doesn't exist
           this->mode.setValue(mode_attr::write);
         else
           this->mode.setValue(mode_attr::read);
      }
      else // only calculate weights() Mode set write but there is no file)
      {
        this->mode.setValue(mode_attr::write);
      }
    }

  }

}
