/*!
   \file attribute_public.hpp
   \author Ha NGUYEN
   \since 27 Oct 2015
   \date 27 Oct 2015

   \brief Class for public attributes
 */

#ifndef __XIOS_ATTRIBUTE_PUBLIC_HPP__
#define __XIOS_ATTRIBUTE_PUBLIC_HPP__

namespace xios
{
/*!
  This class allows to chose whether to expose the attributes in config file to C interface and Fortran Interface
*/
class PublicAttributes
{
public:
  virtual bool isAttributePublic() {return true;}
};
}

#endif // __UTILS_HPP__
