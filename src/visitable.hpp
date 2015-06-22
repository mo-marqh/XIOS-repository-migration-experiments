#ifndef __XIOS_VISITABLE_HPP
#define __XIOS_VISITABLE_HPP

#include "visitor.hpp"
#include <vector>

namespace xios {

template <typename R, typename ParamType>
struct DefaultCatchAll
{
  static R OnUnknownVisitor(ParamType&, CBaseVisitor&)
  { return R(); }
};

////////////////////////////////////////////////////////////////////////////////
// class template CBaseVisitable
////////////////////////////////////////////////////////////////////////////////
template
<
  typename R = void,
  template <typename, class> class CatchAll = DefaultCatchAll,
  bool ConstVisitable = false
>
class CBaseVisitable;

template<typename R,template <typename, class> class CatchAll>
class CBaseVisitable<R, CatchAll, false>
{
public:
  typedef R ReturnType;
  virtual ~CBaseVisitable() {}
  virtual ReturnType apply(CBaseVisitor&) = 0;
  virtual void apply(std::vector<CBaseVisitor*>&) = 0;

protected: // give access only to the hierarchy
  template <class T>
  static ReturnType applyImpl(T& visited, CBaseVisitor& guest)
  {
    // Apply the CVisitor
    if (CVisitor<T,R>* p = dynamic_cast<CVisitor<T,R>*>(&guest))
    {
        return p->operate(visited);
    }
    return CatchAll<R, T>::OnUnknownVisitor(visited, guest);
  }
};

template<typename R,template <typename, class> class CatchAll>
class CBaseVisitable<R, CatchAll, true>
{
public:
  typedef R ReturnType;
  virtual ~CBaseVisitable() {}
  virtual ReturnType apply(CBaseVisitor&) const = 0;
  virtual void apply(std::vector<CBaseVisitor*>&) const = 0;

protected: // give access only to the hierarchy
  template <class T>
  static ReturnType applyImpl(const T& visited, CBaseVisitor& guest)
  {
    // Apply the CVisitor
    if (CVisitor<T,R>* p = dynamic_cast<CVisitor<T,R, true>*>(&guest))
    {
      return p->operate(visited);
    }
    return CatchAll<R, T>::OnUnknownVisitor(const_cast<T&>(visited), guest);
  }
};

////////////////////////////////////////////////////////////////////////////////
/// Put it in every class that you want to make guest
/// functions (in addition to deriving it from CBaseVisitable<R>)
////////////////////////////////////////////////////////////////////////////////
#define DEFINE_VISITABLE() \
    virtual ReturnType apply(::xios::CBaseVisitor& guest) \
    { return applyImpl(*this, guest); }                   \
    virtual void apply(std::vector<CBaseVisitor*>& guests) \
    {                                                             \
      for (std::vector<CBaseVisitor*>::iterator it = guests.begin(); it != guests.end(); ++it) \
        apply(*(*it));                                                                                \
    }
////////////////////////////////////////////////////////////////////////////////
/// Put it in every class that you want to make guest by const member
/// functions (in addition to deriving it from CBaseVisitable<R>)
////////////////////////////////////////////////////////////////////////////////
#define DEFINE_CONST_VISITABLE() \
    virtual ReturnType apply(::xios::CBaseVisitor& guest) const \
    { return applyImpl(*this, guest); }                         \
    virtual void apply(std::vector<CBaseVisitor*>& guests) const \
    {                                                             \
      for (std::vector<CBaseVisitor*>::iterator it = guests.begin(); it != guests.end(); ++it) \
        apply(*(*it));                                                                                \
    }

class CGenericTransformation : public virtual CBaseVisitable<>
{
public:
  DEFINE_VISITABLE()
};

}
#endif // __XIOS_VISITABLE_HPP
