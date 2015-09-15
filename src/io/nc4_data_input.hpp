#ifndef __XIOS_NC4_DATA_INPUT__
#define __XIOS_NC4_DATA_INPUT__

/// XIOS headers ///
#include "xios_spl.hpp"
#include "data_input.hpp"
#include "inetcdf4.hpp"

namespace xios
{
  class CNc4DataInput
    : protected CINetCDF4
    , public virtual CDataInput
  {
  public:
    /// Type definitions ///
    typedef CINetCDF4  SuperClassWriter;
    typedef CDataInput SuperClass;

    /// Constructors ///
    CNc4DataInput(const StdString& filename, MPI_Comm comm_file, bool multifile, bool isCollective = true);
    CNc4DataInput(const CNc4DataInput& dataInput);       // Not implemented.
    CNc4DataInput(const CNc4DataInput* const dataInput); // Not implemented.

    /// Destructor ///
    virtual ~CNc4DataInput(void);

    /// Getters ///
    const StdString& getFileName(void) const;

  protected:
    // Read methods
    virtual StdSize getFieldNbRecords_(CField* field);
    virtual void readFieldData_(CField* field);
    virtual void closeFile_(void);

  private:
    /// Private attributes ///
    MPI_Comm comm_file;
    const StdString filename;
    bool isCollective;
  }; // class CNc4DataInput
} // namespace xios

#endif //__XIOS_NC4_DATA_INPUT__
