#ifndef __XIOS_VISITOR_HPP__
#define __XIOS_VISITOR_HPP__

namespace xios {

class CBaseVisitor
{
public:
    virtual ~CBaseVisitor() {}
};

typedef CBaseVisitor CGenericAlgorithm;

template <class T, typename R = void, bool ConstVisitor = false>
class CVisitor;

template<typename T, typename R>
class CVisitor<T, R, false>
{
public:
    typedef R ReturnType;
    typedef T ParamType;
    virtual ~CVisitor() {}
    virtual ReturnType operate(ParamType&) = 0;
};

template<typename T, typename R>
class CVisitor<T, R, true>
{
public:
    typedef R ReturnType;
    typedef const T ParamType;
    virtual ~CVisitor() {}
    virtual ReturnType operate(ParamType&) = 0;
};

}
#endif // __XIOS_VISITOR_HPP__
