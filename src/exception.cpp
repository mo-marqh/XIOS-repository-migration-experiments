#include "exception.hpp"

/// boost headers ///
#include <boost/cast.hpp>
#include "client.hpp"
#include "server.hpp"
#include "cxios.hpp"

namespace xios
{
  /// ////////////////////// Définitions ////////////////////// ///
   CException::CException(void)
      : CObject(), desc_rethrow(true)
   { /* Ne rien faire de plus */ }

   CException::CException(const StdString & id)
      : CObject(id), desc_rethrow(true)
   { /* Ne rien faire de plus */ }

   CException::CException(const CException & exception)
      : std::basic_ios<char>()
      , CObject(exception.getId())
      , StdOStringStream()
      , desc_rethrow(false)
   { (*this) << exception.str(); }

   CException::~CException(void)
   {
      if (desc_rethrow)
#ifdef __XIOS_NOABORT
      {
        throw (*this);
      }
#else
     {
      int numDigit = 0;
      int size = 0;
      MPI_Comm_size(CXios::globalComm, &size);
      while (size)
      {
        size /= 10;
        ++numDigit;
      }

      StdOFStream fileStream;
      StdStringStream fileNameErr;
      std::streambuf* psbuf;
      if (CXios::isServerSide)
        fileNameErr << CXios::serverFile << "_" << std::setfill('0')
                    << std::setw(numDigit) << CServer::getRank() << ".err";
      else
        fileNameErr << CXios::clientFile << "_" << std::setfill('0')
                    << std::setw(numDigit) << CClient::getRank() << ".err";


      fileStream.open(fileNameErr.str().c_str(), std::ofstream::out);
      psbuf = fileStream.rdbuf();
      std::cerr.rdbuf(psbuf);
      std::cerr << this->getMessage() << std::endl;
      fileStream.close();
      abort();
      }

#endif
   }

   //---------------------------------------------------------------

   StdString CException::getMessage(void) const
   {
      StdOStringStream oss;
      oss << "> Error [" << this->getId() << "] : " << this->str();
      return (oss.str());
   }

   StdOStringStream &  CException::getStream(void)
   { return (*boost::polymorphic_cast<StdOStringStream*>(this)); }

   StdString CException::toString(void) const
   { return (StdString(this->getMessage())); }

   void CException::fromString(const StdString & str)
   { this->str(str); }

   //---------------------------------------------------------------

} // namespace xios
