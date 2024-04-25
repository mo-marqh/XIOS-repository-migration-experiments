#include <string>
#include <boost_uuid.hpp>

namespace xios
{
  
  std::string getUuidStr(void)
  {
    boost_uuid uuid = gen_boost_uuid();
    return to_string(uuid) ;
  }
  
  std::string getUuidStr(const std::string& format)
  {
    size_t pos ;
    std::string retStr(format) ;
    std::string uuid(getUuidStr()) ;
    std::string id("%uuid%");
    pos=retStr.find(id) ;
    while(pos!=std::string::npos)
    {
      retStr.replace(pos,id.size(),uuid) ;
      pos=retStr.find(id) ;
    }
    return retStr;
  }
}

