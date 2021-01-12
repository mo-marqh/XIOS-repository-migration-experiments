#ifndef __XIOS_CTransformation__
#define __XIOS_CTransformation__

#include "xios_spl.hpp"
#include "xml_node.hpp"
#include "transformation_enum.hpp"

namespace xios {

  ///--------------------------------------------------------------
  /*!
    \class CTransformation
    This class describes inverse_axis in xml file.
  */
  class CGenericAlgorithmTransformation;
   class CGrid;

  template<typename T>
  class CTransformation
  {
  public:
    typedef typename std::list<std::pair<ETranformationType, CTransformation<T>* > > TransformationMapTypes;
    typedef TransformationMapTypes TransMapTypes;

    public :
      /// Constructeurs ///
      CTransformation(void) {}
      virtual void checkValid(T* dest) {}

      std::vector<StdString> checkAuxInputs() { return checkAuxInputs_(); }
      static CTransformation<T>* createTransformation(ETranformationType transType, const StdString& id, xml::CXMLNode* node=0);
      static CTransformation<T>* getTransformation(ETranformationType transType, const StdString& id);

      virtual const string& getId(void) = 0 ;
      virtual ETranformationType getTransformationType(void)=0;
      virtual void inheritFrom(CTransformation<T>* src) = 0 ;

      virtual const string& getId_(void) { ERROR("string Transformation<T>::getId())",<< "unimplemented virtual function for child"); } ;
      virtual const string& getName(void) { ERROR("string Transformation<T>::getId())",<< "unimplemented virtual function for child"); } ;
      virtual const string& getDefName(void) { ERROR("string Transformation<T>::getId())",<< "unimplemented virtual function for child"); } ;
      virtual CGenericAlgorithmTransformation* createAlgorithm(bool isSource,
                                                               CGrid* gridDst, CGrid* gridSrc,
                                                               int elementPositionInGrid,
                                                               std::map<int, int>& elementPositionInGridSrc2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridSrc2DomainPosition,
                                                               std::map<int, int>& elementPositionInGridDst2ScalarPosition,
                                                               std::map<int, int>& elementPositionInGridDst2AxisPosition,
                                                               std::map<int, int>& elementPositionInGridDst2DomainPosition) =0 ;

      /// Destructeur ///
    public:
      virtual ~CTransformation(void) {}

    protected:
      typedef CTransformation<T>* (*createTransformationCallBack)(const StdString&, xml::CXMLNode*);
      typedef CTransformation<T>* (*getIdTransformationCallBack)(const StdString&);
      typedef std::map<ETranformationType, tuple<createTransformationCallBack,getIdTransformationCallBack>> callBackMap;
      static callBackMap* callBacks_;

      static bool registerTransformation(ETranformationType transType, tuple<createTransformationCallBack,getIdTransformationCallBack> callBackFunctions);
      static bool unregisterTransformation(ETranformationType transType);

    protected:
      virtual std::vector<StdString> checkAuxInputs_() { return std::vector<StdString>(); }

  }; // class CTransformation

  template<typename T>
  typename CTransformation<T>::callBackMap* CTransformation<T>::callBacks_ = 0; //CTransformation<T>::CallBackMap();

  template<typename T>
  CTransformation<T>* CTransformation<T>::createTransformation(ETranformationType transType, const StdString& id, xml::CXMLNode* node)
  {
    int transTypeInt = transType;
    typename callBackMap::const_iterator it = (*callBacks_).find(transType);
    if ((*callBacks_).end() == it)
    {
       ERROR("CTransformation<T>::createTransformation(ETranformationType transType)",
             << "Transformation type " << transType
             << "doesn't exist. Please define.");
    }
    return (get<0>(it->second))(id,node);
  }

  template<typename T>
  CTransformation<T>* CTransformation<T>::getTransformation(ETranformationType transType, const StdString& id)
  {
    int transTypeInt = transType;
    typename callBackMap::const_iterator it = (*callBacks_).find(transType);
    if ((*callBacks_).end() == it)
    {
       ERROR("CTransformation<T>::getTransformation(ETranformationType transType, const StdString& id)",
             << "Transformation type " << transType
             << "doesn't exist. Please define.");
    }
    return (get<1>(it->second))(id);
  }

  template<typename T>
  bool CTransformation<T>::registerTransformation(ETranformationType transType,  tuple<createTransformationCallBack, getIdTransformationCallBack> functions)
  {
    if (0 == callBacks_) callBacks_ = new callBackMap();
    return (* callBacks_).insert(make_pair(transType, functions)).second;
  }

  template<typename T>
  bool CTransformation<T>::unregisterTransformation(ETranformationType transType)
  {
    int transTypeInt = transType;
    return (1 == (*callBacks_).erase(transType));
  }

} // namespace xios

#endif // __XIOS_CTransformation__
