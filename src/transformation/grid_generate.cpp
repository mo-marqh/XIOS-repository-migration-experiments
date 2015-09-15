/*!
   \file grid_generate.cpp
   \author Ha NGUYEN
   \since 28 Aug 2015
   \date 28 Aug 2015

   \brief A special transformation to generate a grid.
 */
#include "grid_generate.hpp"
#include "domain_algorithm_generate_rectilinear.hpp"
#include "context.hpp"
#include "context_client.hpp"
#include "generate_rectilinear_domain.hpp"

namespace xios {
CGridGenerate::CGridGenerate(CGrid* destination, CGrid* source)
: gridSource_(source), gridDestination_(destination), algoTypes_()
{
  //Verify the compatibity between two grids
  int numElement = gridDestination_->axis_domain_order.numElements();
  if (numElement != gridSource_->axis_domain_order.numElements())
    ERROR("CGridGenerate::CGridGenerate(CGrid* destination, CGrid* source)",
       << "Two grids have different number of elements"
       << "Number of elements of grid source " <<gridSource_->getId() << " is " << gridSource_->axis_domain_order.numElements()  << std::endl
       << "Number of elements of grid destination " <<gridDestination_->getId() << " is " << numElement);

  for (int i = 0; i < numElement; ++i)
  {
    if (gridDestination_->axis_domain_order(i) != gridSource_->axis_domain_order(i))
      ERROR("CGridGenerate::CGridGenerate(CGrid* destination, CGrid* source)",
         << "Transformed grid and its grid source have incompatible elements"
         << "Grid source " <<gridSource_->getId() << std::endl
         << "Grid destination " <<gridDestination_->getId());
  }

  initializeAlgorithms();
}

CGridGenerate::~CGridGenerate()
{
  std::list<CGenericAlgorithmTransformation*>::const_iterator itb = algoTransformation_.begin(), it,
                                                              ite = algoTransformation_.end();
  for (it = itb; it != ite; ++it) delete (*it);
}

/*!
  Initialize the algorithms (transformations)
*/
void CGridGenerate::initializeAlgorithms()
{
  std::vector<int> axisPositionInGrid;
  std::vector<int> domPositionInGrid;
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CDomain*> domListDestP = gridDestination_->getDomains();

  int idx = 0;
  for (int i = 0; i < gridDestination_->axis_domain_order.numElements(); ++i)
  {
    if (false == (gridDestination_->axis_domain_order)(i))
    {
      axisPositionInGrid.push_back(idx);
      ++idx;
    }
    else
    {
      ++idx;
      domPositionInGrid.push_back(idx);
      ++idx;
    }
  }

  for (int i = 0; i < axisListDestP.size(); ++i)
  {
    elementPosition2AxisPositionInGrid_[axisPositionInGrid[i]] = i;
  }

  for (int i = 0; i < domListDestP.size(); ++i)
  {
    elementPosition2DomainPositionInGrid_[domPositionInGrid[i]] = i;
  }

  idx = 0;
  for (int i = 0; i < gridDestination_->axis_domain_order.numElements(); ++i)
  {
    if (false == (gridDestination_->axis_domain_order)(i))
    {
      initializeAxisAlgorithms(idx);
      ++idx;
    }
    else
    {
      ++idx;
      initializeDomainAlgorithms(idx);
      ++idx;
    }
  }
}



/*!
  Initialize the algorithms corresponding to transformation info contained in each axis.
If an axis has transformations, these transformations will be represented in form of vector of CTransformation pointers
In general, each axis can have several transformations performed on itself. However, should they be done seperately or combinely (of course in order)?
For now, one approach is to do these combinely but maybe this needs changing.
\param [in] axisPositionInGrid position of an axis in grid. (for example: a grid with one domain and one axis, position of domain is 1, position of axis is 2)
*/
void CGridGenerate::initializeAxisAlgorithms(int axisPositionInGrid)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  if (!axisListDestP.empty())
  {
    if (axisListDestP[elementPosition2AxisPositionInGrid_[axisPositionInGrid]]->hasTransformation())
    {
      CAxis::TransMapTypes trans = axisListDestP[elementPosition2AxisPositionInGrid_[axisPositionInGrid]]->getAllTransformations();
      CAxis::TransMapTypes::const_iterator itb = trans.begin(), it,
                                           ite = trans.end();
      int transformationOrder = 0;
      for (it = itb; it != ite; ++it)
      {
        listAlgos_.push_back(std::make_pair(axisPositionInGrid, std::make_pair(it->first, transformationOrder)));
        algoTypes_.push_back(false);
        ++transformationOrder;
      }
    }
  }
}

/*!
  Initialize the algorithms corresponding to transformation info contained in each domain.
If a domain has transformations, they will be represented in form of vector of CTransformation pointers
In general, each domain can have several transformations performed on itself.
\param [in] domPositionInGrid position of a domain in grid. (for example: a grid with one domain and one axis, position of domain is 1, position of axis is 2)
*/
void CGridGenerate::initializeDomainAlgorithms(int domPositionInGrid)
{
  std::vector<CDomain*> domListDestP = gridDestination_->getDomains();
  if (!domListDestP.empty())
  {
    if (domListDestP[elementPosition2DomainPositionInGrid_[domPositionInGrid]]->hasTransformation())
    {
      CDomain::TransMapTypes trans = domListDestP[elementPosition2DomainPositionInGrid_[domPositionInGrid]]->getAllTransformations();
      CDomain::TransMapTypes::const_iterator itb = trans.begin(), it,
                                             ite = trans.end();
      int transformationOrder = 0;
      for (it = itb; it != ite; ++it)
      {
        listAlgos_.push_back(std::make_pair(domPositionInGrid, std::make_pair(it->first, transformationOrder)));
        algoTypes_.push_back(true);
        ++transformationOrder;
      }
    }
  }

}

/*!
  Select algorithm correspoding to its transformation type and its position in each element
  \param [in] elementPositionInGrid position of element in grid. e.g: a grid has 1 domain and 1 axis, then position of domain is 1 (because it contains 2 basic elements)
                                             and position of axis is 2
  \param [in] transType transformation type, for now we have Zoom_axis, inverse_axis
  \param [in] transformationOrder position of the transformation in an element (an element can have several transformation)
  \param [in] isDomainAlgo flag to specify type of algorithm (for domain or axis)
*/
void CGridGenerate::selectAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder, bool isDomainAlgo)
{
   if (isDomainAlgo) selectDomainAlgo(elementPositionInGrid, transType, transformationOrder);
   else selectAxisAlgo(elementPositionInGrid, transType, transformationOrder);
}

/*!
  Select algorithm of an axis correspoding to its transformation type and its position in each element
  \param [in] elementPositionInGrid position of element in grid. e.g: a grid has 1 domain and 1 axis, then position of domain is 1 (because it contains 2 basic elements)
                                             and position of axis is 2
  \param [in] transType transformation type, for now we have Zoom_axis, inverse_axis
  \param [in] transformationOrder position of the transformation in an element (an element can have several transformation)
*/
void CGridGenerate::selectAxisAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{
  std::vector<CAxis*> axisListDestP = gridDestination_->getAxis();
  std::vector<CAxis*> axisListSrcP = gridSource_->getAxis();

  int axisIndex =  elementPosition2AxisPositionInGrid_[elementPositionInGrid];
  CAxis::TransMapTypes trans = axisListDestP[axisIndex]->getAllTransformations();
  CAxis::TransMapTypes::const_iterator it = trans.begin();

  for (int i = 0; i < transformationOrder; ++i, ++it) {}  // Find the correct transformation

  CGenericAlgorithmTransformation* algo = 0;
  switch (transType)
  {
    default:
    break;
  }
  algoTransformation_.push_back(algo);

}

/*!
  Select algorithm of a domain correspoding to its transformation type and its position in each element
  \param [in] elementPositionInGrid position of element in grid. e.g: a grid has 1 domain and 1 axis, then position of domain is 1 (because it contains 2 basic elements)
                                             and position of axis is 2
  \param [in] transType transformation type, for now we have Zoom_axis, inverse_axis
  \param [in] transformationOrder position of the transformation in an element (an element can have several transformation)
*/
void CGridGenerate::selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)
{
  std::vector<CDomain*> domainListDestP = gridDestination_->getDomains();
  std::vector<CDomain*> domainListSrcP = gridSource_->getDomains();

  int domainIndex =  elementPosition2DomainPositionInGrid_[elementPositionInGrid];
  CDomain::TransMapTypes trans = domainListDestP[domainIndex]->getAllTransformations();
  CDomain::TransMapTypes::const_iterator it = trans.begin();

  for (int i = 0; i < transformationOrder; ++i, ++it) {}  // Find the correct transformation

  CGenerateRectilinearDomain* genRectDomain = 0;
  CGenericAlgorithmTransformation* algo = 0;
  switch (transType)
  {
    case TRANS_GENERATE_RECTILINEAR_DOMAIN:
      if (0 == transformationOrder)
      {
        genRectDomain = dynamic_cast<CGenerateRectilinearDomain*> (it->second);
        algo = new CDomainAlgorithmGenerateRectilinear(domainListDestP[domainIndex], domainListSrcP[domainIndex], gridSource_, genRectDomain);
      }
      else
      {
         ERROR("CGridGenerate::selectDomainAlgo(int elementPositionInGrid, ETranformationType transType, int transformationOrder)",
           << "Generate rectilinear domain must be the first transformation");
      }
      break;
    default:
      break;
  }
  algoTransformation_.push_back(algo);
}

/*!

*/
void CGridGenerate::completeGrid()
{
  CContext* context = CContext::getCurrent();
  CContextClient* client = context->client;

  ListAlgoType::const_iterator itb = listAlgos_.begin(),
                               ite = listAlgos_.end(), it;
  CGenericAlgorithmTransformation* algo = 0;

  for (it = itb; it != ite; ++it)
  {
    int elementPositionInGrid = it->first;
    ETranformationType transType = (it->second).first;
    int transformationOrder = (it->second).second;

    // First of all, select an algorithm
    selectAlgo(elementPositionInGrid, transType, transformationOrder, algoTypes_[std::distance(itb, it)]);
  }
}

}
