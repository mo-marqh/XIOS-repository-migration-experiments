#include "data_input.hpp"

#include "attribute_template.hpp"
#include "group_template.hpp"
#include "context.hpp"

namespace xios
{
  CDataInput::~CDataInput(void)
  { /* Nothing to do */ }

  //----------------------------------------------------------------

  StdSize CDataInput::getFieldNbRecords(CField* field)
  {
    return this->getFieldNbRecords_(field);
  }

  //----------------------------------------------------------------

  void CDataInput::readFieldData(CField* field, int record, CArray<double,1>& data)
  {
    this->readFieldData_(field, record, data);
  }

  //----------------------------------------------------------------

  void CDataInput::closeFile(void)
  {
    this->closeFile_();
  }

  void CDataInput::readFieldAttributesMetaData(CField* field)
  {
    this->readFieldAttributes_(field, false);
  }

  void CDataInput::readFieldAttributesValues(CField* field)
  {
    this->readFieldAttributes_(field, true);
  }
} // namespace xios
