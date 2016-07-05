#include "scalar.hpp"

#include "attribute_template.hpp"
#include "object_template.hpp"
#include "group_template.hpp"
#include "object_factory.hpp"
#include "xios_spl.hpp"
#include "type.hpp"

namespace xios {

   /// ////////////////////// Définitions ////////////////////// ///

   CScalar::CScalar(void)
      : CObjectTemplate<CScalar>()
      , CScalarAttributes()
      , relFiles()
   { /* Ne rien faire de plus */ }

   CScalar::CScalar(const StdString & id)
      : CObjectTemplate<CScalar>(id)
      , CScalarAttributes()
      , relFiles()
   { /* Ne rien faire de plus */ }

   CScalar::~CScalar(void)
   { /* Ne rien faire de plus */ }

   StdString CScalar::GetName(void)   { return (StdString("scalar")); }
   StdString CScalar::GetDefName(void){ return (CScalar::GetName()); }
   ENodeType CScalar::GetType(void)   { return (eScalar); }

   CScalar* CScalar::createScalar()
   {
     CScalar* scalar = CScalarGroup::get("scalar_definition")->createChild();
     return scalar;
   }

   bool CScalar::IsWritten(const StdString & filename) const
   {
      return (this->relFiles.find(filename) != this->relFiles.end());
   }

   void CScalar::addRelFile(const StdString& filename)
   {
      this->relFiles.insert(filename);
   }

   void CScalar::checkAttributes(void)
   {
//      if (this->value.isEmpty())
//      {
//        this->value.setValue(0);
//      }
   }

  void CScalar::checkAttributesOnClient()
  {

  }

  CTransformation<CScalar>* CScalar::addTransformation(ETranformationType transType, const StdString& id)
  {
    transformationMap_.push_back(std::make_pair(transType, CTransformation<CScalar>::createTransformation(transType,id)));
    return transformationMap_.back().second;
  }

  bool CScalar::hasTransformation()
  {
    return (!transformationMap_.empty());
  }

  void CScalar::setTransformations(const TransMapTypes& scalarTrans)
  {
    transformationMap_ = scalarTrans;
  }

  CScalar::TransMapTypes CScalar::getAllTransformations(void)
  {
    return transformationMap_;
  }

  /*!
    Check the validity of all transformations applied on scalar
  This functions is called AFTER all inherited attributes are solved
  */
  void CScalar::checkTransformations()
  {
    TransMapTypes::const_iterator itb = transformationMap_.begin(), it,
                                  ite = transformationMap_.end();
    for (it = itb; it != ite; ++it)
    {
      (it->second)->checkValid(this);
    }
  }

  void CScalar::duplicateTransformation(CScalar* src)
  {
    if (src->hasTransformation())
    {
      this->setTransformations(src->getAllTransformations());
    }
  }

  /*!
   * Go through the hierarchy to find the scalar from which the transformations must be inherited
   */
  void CScalar::solveInheritanceTransformation()
  {
    if (hasTransformation() || !hasDirectScalarReference())
      return;

    CScalar* scalar = this;
    std::vector<CScalar*> refScalar;
    while (!scalar->hasTransformation() && scalar->hasDirectScalarReference())
    {
      refScalar.push_back(scalar);
      scalar = scalar->getDirectScalarReference();
    }

    if (scalar->hasTransformation())
      for (size_t i = 0; i < refScalar.size(); ++i)
        refScalar[i]->setTransformations(scalar->getAllTransformations());
  }


  // Definition of some macros
  DEFINE_REF_FUNC(Scalar,scalar)

} // namespace xios
