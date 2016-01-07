#include "nc4_data_input.hpp"

#include "context.hpp"
#include "context_server.hpp"
#include "context_client.hpp"
#include "domain.hpp"
#include "axis.hpp"

namespace xios
{
  CNc4DataInput::CNc4DataInput(const StdString& filename, MPI_Comm comm_file, bool multifile, bool isCollective /*= true*/, const StdString& timeCounterName /*= "time_counter"*/)
    : SuperClass()
    , SuperClassWriter(filename, &comm_file, multifile, timeCounterName)
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
//      return SuperClassWriter::getDimensions(&fieldId)[SuperClassWriter::getUnlimitedDimensionName()];
      return SuperClassWriter::getDimensions(&fieldId)[SuperClassWriter::getTimeCounterName()];
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
    bool isUnstructuredGrid = SuperClassWriter::isUnstructured(fieldId);
    std::map<StdString, StdSize> dimSizeMap = SuperClassWriter::getDimensions(&fieldId);
    std::list<StdString> dimList = SuperClassWriter::getDimensionsList(&fieldId);
    
    realGridDim = SuperClassWriter::isTemporal(fieldId) ? dimSizeMap.size() - 1 : dimSizeMap.size();
    if (isUnstructuredGrid) ++realGridDim;

    if (gridDim != realGridDim)
       ERROR("CNc4DataInput::readFieldAttributes_(CField* field, bool readAttributeValues)",
        << "Field '" << fieldId << "' has incorrect dimension " << std::endl
        << "Verify dimension of grid defined by 'grid_ref' or 'domain_ref'/'axis_ref' and dimension of grid in read file.");

    // Remove unlimited dimension from the map, we dont need it anymore
    if (SuperClassWriter::isTemporal(fieldId)) 
    {
      dimSizeMap.erase(SuperClassWriter::getUnlimitedDimensionName());
      dimList.pop_front() ;  // assume time dimension is first
    }
    
    int mapSize = dimSizeMap.size() - 1;
    std::list<std::pair<StdString, StdSize> > listDimSize;
/*
    for (std::map<StdString, StdSize>::const_iterator itMap = dimSizeMap.begin(); itMap != dimSizeMap.end(); ++itMap)
      listDimSize.push_front(*itMap);
*/
    for (std::list<StdString>::const_iterator it = dimList.begin(); it != dimList.end(); ++it)
      listDimSize.push_front(*dimSizeMap.find(*it));

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
           if (it.second) readDomainAttributeValueFromFile(domainP[idxDomain], listDimSize, elementPosition, fieldId);
        }
        else
        {
          it = readMetaDataDomains_.insert(domainP[idxDomain]->getId());
          if (it.second) readDomainAttributesFromFile(domainP[idxDomain], listDimSize, elementPosition, fieldId);
        }
        ++idxDomain;
        if (isUnstructuredGrid) ++elementPosition;
        else elementPosition += 2;
      }
      else
      {
        if (readAttributeValues)
        {
          it = readValueAxis_.insert(axisP[idxAxis]->getId());
          if (it.second) readAxisAttributeValueFromFile(axisP[idxAxis], listDimSize, elementPosition, fieldId);
        }
        else
        {
          it = readMetaDataAxis_.insert(axisP[idxAxis]->getId());
          if (it.second) readAxisAttributesFromFile(axisP[idxAxis], listDimSize, elementPosition, fieldId);
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
    \param [in] fieldId id (or name) associated with the grid
  */
  void CNc4DataInput::readDomainAttributeValueFromFile(CDomain* domain, std::list<std::pair<StdString, StdSize> >& dimSizeMap,
                                                       int elementPosition, const StdString& fieldId)
  {
    // There are some optional attributes of a domain to retrieve from file    // + lon lat?
    std::list<std::pair<StdString, StdSize> >::const_iterator itMapNi = dimSizeMap.begin(), itMapNj,
                                                              iteMap  = dimSizeMap.end();

    for (int i = 0; i < elementPosition; ++i, ++itMapNi) {}
    itMapNj = itMapNi; ++itMapNj;

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
      int ni = domain->ni;
      int nj = domain->nj;
      std::vector<StdSize> nBeginLatLon(2), nSizeLatLon(2);
      nBeginLatLon[0] = domain->jbegin.getValue(); nBeginLatLon[1] = domain->ibegin.getValue();
      nSizeLatLon[0]  = nj; nSizeLatLon[1] = ni;

      StdString latName = this->getLatCoordName(fieldId);
      domain->latvalue_2d.resize(ni,nj);
      readFieldVariableValue(domain->latvalue_2d, latName, nBeginLatLon, nSizeLatLon);
      StdString lonName = this->getLonCoordName(fieldId);
      domain->lonvalue_2d.resize(ni,nj);
      readFieldVariableValue(domain->lonvalue_2d, lonName, nBeginLatLon, nSizeLatLon);

      StdString boundsLatName = this->getBoundsId(latName);
      if (0 == boundsLatName.compare(""))
         ERROR("CNc4DataInput::readDomainAttributeValueFromFile(...)",
              << "Field '" << fieldId << std::endl
              << "Trying to read attributes from curvilinear grid."
              << "Latitude variable " << latName << " does not have bounds.");
      StdString boundsLonName = this->getBoundsId(lonName);
      if (0 == boundsLonName.compare(""))
         ERROR("CNc4DataInput::readDomainAttributeValueFromFile(...)",
              << "Field '" << fieldId << std::endl
              << "Trying to read attributes from curvilinear grid."
              << "Longitude variable " << lonName << " does not have bounds.");

      int nbVertex = this->getNbVertex(fieldId);
      domain->nvertex.setValue(nbVertex);
      std::vector<StdSize> nBeginBndsLatLon(3), nSizeBndsLatLon(3);
      nBeginBndsLatLon[0] = domain->jbegin.getValue(); nSizeBndsLatLon[0] = nj;
      nBeginBndsLatLon[1] = domain->ibegin.getValue(); nSizeBndsLatLon[1] = ni;
      nBeginBndsLatLon[2] = 0; nSizeBndsLatLon[2] = nbVertex;

      domain->bounds_lat_2d.resize(nbVertex,ni,nj);
      readFieldVariableValue(domain->bounds_lat_2d, boundsLatName, nBeginBndsLatLon, nSizeBndsLatLon);
      domain->bounds_lon_2d.resize(nbVertex,ni,nj);
      readFieldVariableValue(domain->bounds_lon_2d, boundsLonName, nBeginBndsLatLon, nSizeBndsLatLon);
    }
    else if (this->isUnstructured(fieldId))
    {
      /*
      if (domain->i_index.isEmpty())
         ERROR("CNc4DataInput::readDomainAttributeValueFromFile(...)",
              << "Field '" << fieldId << std::endl
              << "Trying to read attributes from unstructured grid."
              << "i_index of domain" << domain->getId() << " is mandatory");
      
      int ni = domain->i_index.numElements();
*/
      int ni     = domain->ni;
      int ibegin = domain->ibegin;
      if (domain->i_index.isEmpty())
      {
        domain->i_index.resize(ni) ;
        for(int idx = 0; idx < ni; ++idx) domain->i_index(idx)=ibegin+idx ;
      }
      
      std::vector<StdSize> nBeginLatLon(1,0), nSizeLatLon(1,0);
      nSizeLatLon[0]  = domain->ni_glo.getValue();
      CArray<double,1> globalLonLat(domain->ni_glo.getValue());

      StdString latName = this->getLatCoordName(fieldId);
      readFieldVariableValue(globalLonLat, latName, nBeginLatLon, nSizeLatLon);
      domain->latvalue_1d.resize(ni);
      for (int idx = 0; idx < ni; ++idx)
        domain->latvalue_1d(idx) =  globalLonLat(domain->i_index(idx));

      StdString lonName = this->getLonCoordName(fieldId);
      readFieldVariableValue(globalLonLat, lonName, nBeginLatLon, nSizeLatLon);
      domain->lonvalue_1d.resize(ni);
      for (int idx = 0; idx < ni; ++idx)
        domain->lonvalue_1d(idx) = globalLonLat(domain->i_index(idx));

      StdString boundsLatName = this->getBoundsId(latName);
      if (0 == boundsLatName.compare(""))
         ERROR("CNc4DataInput::readDomainAttributeValueFromFile(...)",
              << "Field '" << fieldId << std::endl
              << "Trying to read attributes from unstructured grid."
              << "Latitude variable " << latName << " does not have bounds.");
      StdString boundsLonName = this->getBoundsId(lonName);
      if (0 == boundsLonName.compare(""))
         ERROR("CNc4DataInput::readDomainAttributeValueFromFile(...)",
              << "Field '" << fieldId << std::endl
              << "Trying to read attributes from unstructured grid."
              << "Longitude variable " << lonName << " does not have bounds.");

      int nbVertex = this->getNbVertex(fieldId);
      domain->nvertex.setValue(nbVertex);
      std::vector<StdSize> nBeginBndsLatLon(2), nSizeBndsLatLon(2);
      nBeginBndsLatLon[0] = 0; nSizeBndsLatLon[0] = domain->ni_glo.getValue();
      nBeginBndsLatLon[1] = 0; nSizeBndsLatLon[1] = nbVertex;

      CArray<double,2> globalBndsLonLat(nSizeBndsLatLon[1], nSizeBndsLatLon[0]);
      readFieldVariableValue(globalBndsLonLat, boundsLatName, nBeginBndsLatLon, nSizeBndsLatLon);
      domain->bounds_lat_1d.resize(nbVertex,ni);
      for (int idx = 0; idx < ni; ++idx)
        for (int jdx = 0; jdx < nbVertex; ++jdx)
          domain->bounds_lat_1d(jdx,idx) = globalBndsLonLat(jdx, domain->i_index(idx));

      readFieldVariableValue(globalBndsLonLat, boundsLonName, nBeginBndsLatLon, nSizeBndsLatLon);
      domain->bounds_lon_1d.resize(nbVertex,ni);
      for (int idx = 0; idx < ni; ++idx)
        for (int jdx = 0; jdx < nbVertex; ++jdx)
          domain->bounds_lon_1d(jdx,idx) = globalBndsLonLat(jdx, domain->i_index(idx));
    }
  }

  /*!
    Read attribute value of a domain from a file
    \param [in] domain domain whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of domain in grid
    \param [in] fieldId id (or name) associated with the grid
  */
  void CNc4DataInput::readDomainAttributesFromFile(CDomain* domain, std::list<std::pair<StdString, StdSize> >& dimSizeMap,
                                                   int elementPosition, const StdString& fieldId)
  {
    // There are some mandatory attributes of a domain to retrieve from file
    // + ni_glo, nj_glo
    std::list<std::pair<StdString, StdSize> >::const_iterator itMapNi = dimSizeMap.begin(), itMapNj,
                                                              iteMap  = dimSizeMap.end();
    for (int i = 0; i < elementPosition; ++i, ++itMapNi) {}
    itMapNj = itMapNi; ++itMapNj;

    if (this->isRectilinear(fieldId) || this->isCurvilinear(fieldId))
    {
      domain->nj_glo.setValue(itMapNj->second);
      domain->ni_glo.setValue(itMapNi->second);
    }
    else if (this->isUnstructured(fieldId))
    {
      domain->nj_glo.setValue(1);
      domain->ni_glo.setValue(itMapNi->second);
    }
  }

  /*!
    Read attributes of an axis from a file
    \param [in] axis axis whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of axis in grid
    \param [in] fieldId id (or name) associated with the grid
  */
  void CNc4DataInput::readAxisAttributesFromFile(CAxis* axis, std::list<std::pair<StdString, StdSize> >& dimSizeMap,
                                                 int elementPosition, const StdString& fieldId)
  {
    std::list<std::pair<StdString, StdSize> >::const_iterator itMapN = dimSizeMap.begin(),
                                                              iteMap = dimSizeMap.end();
    for (int i = 0; i < elementPosition; ++i, ++itMapN) {}
    axis->n_glo.setValue(itMapN->second);
  }

  /*!
    Read attribute value of an axis from a file
    \param [in] axis axis whose attributes are read from the file
    \param [in] dimSizeMap Dimensions and and their corresponding names and size read from file
    \param [in] emelentPosition position of axis in grid
    \param [in] fieldId id (or name) associated with the grid
  */
  void CNc4DataInput::readAxisAttributeValueFromFile(CAxis* axis, std::list<std::pair<StdString, StdSize> >& dimSizeMap,
                                                    int elementPosition, const StdString& fieldId)
  {
    std::list<std::pair<StdString, StdSize> >::const_iterator itMapN = dimSizeMap.begin(),
                                                              iteMap = dimSizeMap.end();
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

  void CNc4DataInput::closeFile_(void)
  {
    SuperClassWriter::close();
  }
} // namespace xios
