#include "nc4_data_input.hpp"

#include "context.hpp"
#include "context_server.hpp"

namespace xios
{
  CNc4DataInput::CNc4DataInput(const StdString& filename, MPI_Comm comm_file, bool multifile, bool isCollective /*= true*/)
    : SuperClass()
    , SuperClassWriter(filename, &comm_file, multifile)
    , comm_file(comm_file)
    , filename(filename)
    , isCollective(isCollective)
  {
    SuperClass::type = multifile ? MULTI_FILE : ONE_FILE;
  }

  CNc4DataInput::~CNc4DataInput(void)
  { /* Nothing more to do */ }

  StdSize CNc4DataInput::getFieldNbRecords_(CField* field)
  {
    StdString fieldId = !field->name.isEmpty() ? field->name.getValue() : field->getBaseFieldReference()->getId();

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

    StdString fieldId = !field->name.isEmpty() ? field->name.getValue() : field->getBaseFieldReference()->getId();

    CArray<double,1> fieldData(grid->getWrittenDataSize());
    if (!field->default_value.isEmpty()) fieldData = field->default_value;

    switch (SuperClass::type)
    {
      case MULTI_FILE:
        SuperClassWriter::getData(fieldData, fieldId, isCollective, field->getNStep() - 1);
        break;
      case ONE_FILE:
      {
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

  void CNc4DataInput::closeFile_(void)
  {
    SuperClassWriter::close();
  }
} // namespace xios
