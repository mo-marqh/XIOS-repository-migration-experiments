#include "nc4_data_input.hpp"

#include "context.hpp"
#include "context_server.hpp"
#include "context_client.hpp"
#include "domain.hpp"
#include "axis.hpp"

namespace xios
{
  CNc4DataInput::CNc4DataInput(const StdString& filename, MPI_Comm comm_file, bool multifile, bool isCollective /*= true*/)
    : SuperClass()
    , SuperClassWriter(filename, &comm_file, multifile)
    , comm_file(comm_file)
    , filename(filename)
    , isCollective(isCollective)
    , readMetaDataDomains_(), readValueDomains_()
    , readMetaDataAxis_(), readValueAxis_()
  {
    SuperClass::type = multifile ? MULTI_FILE : ONE_FILE;
  }

  CNc4DataInput::~CNc4DataInput(void)
  { /* Nothing more to do */ }

  StdSize CNc4DataInput::getFieldNbRecords_(CField* field)
  {
    StdString fieldId = field->getFieldOutputName();

    if (SuperClassWriter::isTemporal(fieldId))
    {
      return SuperClassWriter::getDimensions(&fieldId)[SuperClassWriter::getUnlimitedDimensionName()];
    }

    return 1;
  }

  void CNc4DataInput::readFieldData_(CField* field)
  {
    CContext* context = CContext::getCurrent();
    CContextServer* server = context->server;

    CGrid* grid = field->grid;

    if (!grid->doGridHaveDataToWrite())
      if (SuperClass::type==MULTI_FILE || !isCollective) return;

    StdString fieldId = field->getFieldOutputName();

    CArray<double,1> fieldData(grid->getWrittenDataSize());
    if (!field->default_value.isEmpty()) fieldData = field->default_value;

    switch (SuperClass::type)
    {
      case MULTI_FILE:
        SuperClassWriter::getData(fieldData, fieldId, isCollective, field->getNStep() - 1);
        break;
      case ONE_FILE:
      {
/*
        std::vector<int> nZoomBeginGlobal = grid->getDistributionServer()->getZoomBeginGlobal();
        std::vector<int> nZoomBeginServer = grid->getDistributionServer()->getZoomBeginServer();
        std::vector<int> nZoomSizeServer  = grid->getDistributionServer()->getZoomSizeServer();

        int ssize = nZoomBeginGlobal.size();

        std::vector<StdSize> start(ssize);
        std::vector<StdSize> count(ssize);

        for (int i = 0; i < ssize; ++i)
        {
          start[i] = nZoomBeginServer[ssize - i - 1] - nZoomBeginGlobal[ssize - i - 1];
          count[i] = nZoomSizeServer[ssize - i - 1];
        }
*/

        std::vector<int> nZoomBeginGlobal = grid->getDistributionServer()->getZoomBeginGlobal();
        std::vector<int> nZoomBeginServer = grid->getDistributionServer()->getZoomBeginServer();
        std::vector<int> nZoomSizeServer  = grid->getDistributionServer()->getZoomSizeServer();

        std::vector<StdSize> start, count;

        CArray<bool,1> axisDomainOrder = grid->axis_domain_order;
        std::vector<StdString> domainList = grid->getDomainList();
        std::vector<StdString> axisList   = grid->getAxisList();
        int numElement = axisDomainOrder.numElements();
        int idxDomain = domainList.size() - 1, idxAxis = axisList.size() - 1;
        int idx = nZoomBeginGlobal.size() - 1;

        start.reserve(nZoomBeginGlobal.size());
        count.reserve(nZoomBeginGlobal.size());

        for (int i = numElement - 1; i >= 0; --i)
        {
          if (axisDomainOrder(i))
          {
            CDomain* domain = CDomain::get(domainList[idxDomain]);
            if ((domain->type) != CDomain::type_attr::unstructured)
            {
              start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
              count.push_back(nZoomSizeServer[idx]);
            }
            --idx ;
            start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
            count.push_back(nZoomSizeServer[idx]);
            --idx ;
            --idxDomain;
          }
          else
          {
            start.push_back(nZoomBeginServer[idx] - nZoomBeginGlobal[idx]);
            count.push_back(nZoomSizeServer[idx]);
            --idx;
           }
        }

        SuperClassWriter::getData(fieldData, fieldId, isCollective, field->getNStep() - 1, &start, &count);
        break;
      }
    }

    field->inputField(fieldData);

    if (!field->scale_factor.isEmpty() || !field->add_offset.isEmpty())
    {
      double scaleFactor = 1.0, addOffset = 0.0;
      if (!field->scale_factor.isEmpty()) scaleFactor = field->scale_factor;
      if (!field->add_offset.isEmpty()) addOffset = field->add_offset;
      field->invertScaleFactorAddOffset(scaleFactor, addOffset);
    }
  }

  void CNc4DataInput::readFieldAttributes_(CField* field, bool readAttributeValues)
  {
    StdString fieldId = field->getFieldOutputName();

    CGrid* grid = field->grid;

    std::vector<CDomain*> domainP = grid->getDomains();
    std::vector<CAxis*> axisP = grid->getAxis();
    int gridDim = domainP.size() * 2 + axisP.size();

    // Verify the compatibility of dimension of declared grid and real grid in file
    int realGridDim = 1;
    std::map<StdString, StdSize> dimSizeMap = SuperClassWriter::getDimensions(&fieldId);
    realGridDim = SuperClassWriter::isTemporal(fieldId) ? dimSizeMap.size() - 1 : dimSizeMap.size();

    if (gridDim != realGridDim)
       ERROR("CNc4DataInput::readFieldAttributes_(CField* field, bool readAttributeValues)",
        << "Field '" << fieldId << "' has incorrect dimension " << std::endl
        << "Verify dimension of grid defined by 'grid_ref' or 'domain_ref'/'axis_ref' and dimension of grid in read file.");

    // Remove unlimited dimension from the map, we dont need it anymore
    if (SuperClassWriter::isTemporal(fieldId)) dimSizeMap.erase(SuperClassWriter::getUnlimitedDimensionName());
    int mapSize = dimSizeMap.size() - 1;

    // Now process domain and axis
    CArray<bool,1> axisDomainOrder = grid->axis_domain_order;
    int numElement = domainP.size() + axisP.size();
    int elementPosition = 0;
    int idxDomain = 0, idxAxis = 0;

    std::pair<std::set<StdString>::iterator,bool> it;
    for (int i = 0; i < numElement; ++i)
    {
      if(axisDomainOrder(i))
      {
        if (readAttributeValues)
        {
           it = readValueDomains_.insert(domainP[idxDomain]->getId());
           if (it.second) readDomainAttributeValueFromFile(domainP[idxDomain], dimSizeMap, mapSize - 1 - elementPosition, fieldId);
        }
        else
        {
          it = readMetaDataDomains_.insert(domainP[idxDomain]->getId());
          if (it.second) readDomainAttributesFromFile(domainP[idxDomain], dimSizeMap, mapSize - 1 - elementPosition, fieldId);
        }
        ++idxDomain;
        elementPosition += 2;
      }
      else
      {
        if (readAttributeValues)
        {
          it = readValueAxis_.insert(axisP[idxAxis]->getId());
          if (it.second) readAxisAttributeValueFromFile(axisP[idxAxis], dimSizeMap, mapSize - elementPosition, fieldId);
        }
        else
        {
          it = readMetaDataAxis_.insert(axisP[idxAxis]->getId());
          if (it.second) readAxisAttributesFromFile(axisP[idxAxis], dimSizeMap, mapSize - elementPosition, fieldId);
        }
        ++idxAxis;
        ++elementPosition;
      }
    }
  }

  /*!
    Read attributes of a domain from a file
    \param [in] domain domain whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of domain in grid
  */
  void CNc4DataInput::readDomainAttributeValueFromFile(CDomain* domain, std::map<StdString, StdSize>& dimSizeMap,
                                                       int elementPosition, const StdString& fieldId)
  {
    // There are some optional attributes of a domain to retrieve from file    // + lon lat?
    std::map<StdString, StdSize>::const_iterator itMapNj = dimSizeMap.begin(), itMapNi,
                                                 iteMap  = dimSizeMap.end();

    for (int i = 0; i < elementPosition; ++i, ++itMapNj) {}
    itMapNi = itMapNj; ++itMapNi;

    if (this->isRectilinear(fieldId))
    {
      // Ok, try to read some f.. attributes such as longitude and latitude
      domain->latvalue_rectilinear_read_from_file.resize(itMapNj->second);
      std::vector<StdSize> nBeginLat(1, 0), nSizeLat(1, itMapNj->second);
      readFieldVariableValue(domain->latvalue_rectilinear_read_from_file, itMapNj->first, nBeginLat, nSizeLat, true);

      domain->lonvalue_rectilinear_read_from_file.resize(itMapNi->second);
      std::vector<StdSize> nBeginLon(1, 0), nSizeLon(1, itMapNi->second);
      readFieldVariableValue(domain->lonvalue_rectilinear_read_from_file, itMapNi->first, nBeginLon, nSizeLon, true);
      domain->fillInRectilinearLonLat();
    }
    else if (this->isCurvilinear(fieldId))
    {

    }
    else if (this->isUnstructured(fieldId))
    {

    }
  }

  /*!
    Read attributes of a domain from a file
    \param [in] domain domain whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of domain in grid
  */
  void CNc4DataInput::readDomainAttributesFromFile(CDomain* domain, std::map<StdString, StdSize>& dimSizeMap,
                                                   int elementPosition, const StdString& fieldId)
  {
    // There are some mandatory attributes of a domain to retrieve from file
    // + ni_glo, nj_glo
    std::map<StdString, StdSize>::const_iterator itMapNj = dimSizeMap.begin(), itMapNi,
                                                 iteMap  = dimSizeMap.end();
    for (int i = 0; i < elementPosition; ++i, ++itMapNj) {}
    itMapNi = itMapNj; ++itMapNi;

    if (this->isRectilinear(fieldId))
    {
      domain->nj_glo.setValue(itMapNj->second);
      domain->ni_glo.setValue((itMapNi)->second);
    }
    else if (this->isCurvilinear(fieldId))
    {

    }
    else if (this->isUnstructured(fieldId))
    {

    }
  }

  /*!
    Read attributes of an axis from a file
    \param [in] axis axis whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of axis in grid
  */
  void CNc4DataInput::readAxisAttributesFromFile(CAxis* axis, std::map<StdString, StdSize>& dimSizeMap,
                                                 int elementPosition, const StdString& fieldId)
  {
    std::map<StdString, StdSize>::const_iterator itMapN = dimSizeMap.begin(),
                                                 iteMap  = dimSizeMap.end();
    for (int i = 0; i < elementPosition; ++i, ++itMapN) {}
    axis->n_glo.setValue(itMapN->second);
  }

  /*!
    Read attributes of an axis from a file
    \param [in] axis axis whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of axis in grid
  */
  void CNc4DataInput::readAxisAttributeValueFromFile(CAxis* axis, std::map<StdString, StdSize>& dimSizeMap,
                                                    int elementPosition, const StdString& fieldId)
  {
    std::map<StdString, StdSize>::const_iterator itMapN = dimSizeMap.begin(),
                                                 iteMap  = dimSizeMap.end();
    for (int i = 0; i < elementPosition; ++i, ++itMapN) {}

    { // Read axis value
      std::vector<StdSize> nBegin(1, 0), nSize(1, itMapN->second);
      CArray<double,1> readAxisValue(itMapN->second);
      readFieldVariableValue(readAxisValue, itMapN->first, nBegin, nSize, true);
      int begin = 0, n = itMapN->second;
      if (!axis->begin.isEmpty()) begin = axis->begin.getValue();
      if (!axis->n.isEmpty()) n = axis->n.getValue();
      axis->value.resize(n);
      for (int i = 0; i < n; ++i) axis->value(i) = readAxisValue(begin + i);
    }
  }

  void CNc4DataInput::readFieldVariableValue(CArray<double,1>& var, const StdString& varId,
                                             const std::vector<StdSize>& nBegin,
                                             const std::vector<StdSize>& nSize,
                                             bool forceIndependent)
  {
    if (SuperClass::type==MULTI_FILE || !isCollective) return;

    bool openCollective = isCollective;
    if (forceIndependent) openCollective = !isCollective;
    switch (SuperClass::type)
    {
      case MULTI_FILE:
        SuperClassWriter::getData(var, varId, openCollective, 0);
        break;
      case ONE_FILE:
      {
        SuperClassWriter::getData(var, varId, openCollective, 0, &nBegin, &nSize);
        break;
      }
    }
  }

  void CNc4DataInput::closeFile_(void)
  {
    SuperClassWriter::close();
  }
} // namespace xios
