#include "expr_node.hpp"
#include "field.hpp"

namespace xios
{

  CScalarNode* CScalarNode::newNode(CSimpleNodeExpr* simpleNode)
  {
    if (simpleNode->nodeType==CSimpleNodeExpr::scalarDouble) return new CScalarDouble(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::scalarVariable) return new CScalarVariable(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opScalar) return new COperatorScalarNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opScalarScalar) return new COperatorScalarScalarNode(simpleNode) ;
    else 
    {
      ERROR("CScalarNode* CScalarNode::allocateChild(CSimpleNodeExpr* simpleNode)",<<"Non coherent node")
      return NULL;
    }
  };


  CFieldNode* CFieldNode::newNode(CSimpleNodeExpr* simpleNode)
  {
    if (simpleNode->nodeType==CSimpleNodeExpr::fieldInstant) return new CInstantFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::fieldAverage) return new  CAverageFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opFieldScalar) return new COperatorFieldScalarNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opScalarField) return new COperatorScalarFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opFieldField) return new COperatorFieldFieldNode(simpleNode) ;
    else if (simpleNode->nodeType==CSimpleNodeExpr::opField) return new COperatorFieldNode(simpleNode) ;
    else 
    {
      ERROR("CScalarNode* CScalarNode::allocateChild(CSimpleNodeExpr* simpleNode)",<<"Non coherent node")
      return NULL;
    }
  };

  void CInstantFieldNode::reduce(CField* thisField)
  {
    if (!reduced)
    {
      if (fieldId=="this")
      {
        field=thisField ;
        array=thisField->getInstantData() ;
        reduced=true ;
      }
      else if (CField::has(fieldId)) 
      {
        field =CField::get(fieldId) ;
        array=field->getInstantData() ;
        reduced=true ;
      }
      else ERROR("void CInstantFieldNode::reduce(void)",<<" Field "<<fieldId<<" does not exist")
      }
  }   

  void CAverageFieldNode::reduce(CField* thisField)
  {
    if (!reduced)
    {
      if (CField::has(fieldId)) 
      {
        field=thisField ;
        array=CField::get(fieldId)->getInstantData() ;
        reduced=true ;
      }
      else ERROR("void CAverageFieldNode::reduce(void)",<<" Field "<<fieldId<<" does not exist")
      }
  }
  
}
