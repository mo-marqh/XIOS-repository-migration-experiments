#include "transformation.hpp"
#include "type.hpp"
#include "duration.hpp"
#include "context.hpp"
#include <iterator>

namespace xios {

  /// ////////////////////// Définitions ////////////////////// ///

  CTransformation::CTransformation(void)
    : CObjectTemplate<CTransformation>(), CTransformationAttributes()
  { /* Ne rien faire de plus */ }

  CTransformation::CTransformation(const StdString & id)
    : CObjectTemplate<CTransformation>(id), CTransformationAttributes()
  { /* Ne rien faire de plus */ }

  CTransformation::~CTransformation(void)
  {}

  //----------------------------------------------------------------

  StdString CTransformation::GetName(void)    { return StdString("transformation"); }
  StdString CTransformation::GetDefName(void) { return StdString("transformation"); }
  ENodeType CTransformation::GetType(void)    { return eTransformation; }

  StdString transTypeTmp[] = {"inverse", "zoom" };
  CTransformation::TransformationId transTypeIdTmp[] = {CTransformation::TRANS_ID_INVERSE, CTransformation::TRANS_ID_ZOOM};
  std::vector<StdString> CTransformation::TransformationTypes = std::vector<StdString>(transTypeTmp, transTypeTmp + sizeof(transTypeTmp) / sizeof(StdString));
  std::vector<CTransformation::TransformationId> CTransformation::TransformationTypeIds = std::vector<CTransformation::TransformationId>(transTypeIdTmp, transTypeIdTmp + sizeof(transTypeIdTmp) / sizeof(CTransformation::TransformationId));
  //----------------------------------------------------------------

  void CTransformation::checkAttributes()
  {
    if (this->type.isEmpty())
      ERROR("CTransformation::checkAttributes(void)",
             << "Attribute <type> of the transformation in the context = '"
             << CObjectFactory::GetCurrentContextId() << "' ] must be specified");
    StdString transformationTypeStr = this->type.getValue();

    std::vector<StdString>::const_iterator itbTrans = TransformationTypes.begin(), itTrans,
                                           iteTrans = TransformationTypes.end();
    itTrans = std::find(itbTrans, iteTrans,transformationTypeStr);
    if (TransformationTypes.end() == itTrans)
    {
      StdString msgTmp;
      for (std::vector<StdString>::const_iterator it = itbTrans; it != iteTrans; ++it)
        msgTmp += *it + ", ";
      ERROR("CTransformation::checkAttributes(void)",
            << "Attribute <type = " << transformationTypeStr << " > is invalid"
            << "It should be one of following: " << msgTmp);
    }

//    if (this->axis_ref.isEmpty() && this->domain_ref.isEmpty())
//      ERROR("CTransformation::checkAttributes(void)",
//            << "Attribute <axis_ref> or <domain_ref> of the transformation in the context = '"
//            << CObjectFactory::GetCurrentContextId() << "' ] must be specified");


    checkAttributesType(TransformationTypeIds[std::distance(itbTrans, itTrans)]);
  }

  void CTransformation::checkAttributesType(TransformationId& typeId)
  {
    switch (typeId) {
    case TRANS_ID_ZOOM:
      checkAttributesZoomType();
      break;
    case TRANS_ID_INVERSE:
      checkAttributesInverseType();
      break;
    default:
      ERROR("CTransformation::checkAttributesType(TransformationId& typeId)",
            << "Type = " << typeId << " is invalid");
    }
  }

  void CTransformation::checkAttributesZoomType()
  {

  }

  void CTransformation::checkAttributesInverseType()
  {

  }

}
