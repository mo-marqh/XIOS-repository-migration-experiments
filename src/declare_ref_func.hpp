/*!
   \file declare_ref_func.hpp
   \author Ha NGUYEN
   \date 02 Dec 2014
   \since 02 Dec 2014

   \brief Macros to add functions used to solve references in class CDomain, CAxis and CField
 */
#ifndef __XIOS_DECLARE_REF_FUNC_HPP__
#define __XIOS_DECLARE_REF_FUNC_HPP__

// Declarations

#define DECLARE_REF_FUNC(type, name)                        \
public:                                                     \
  bool hasDirect##type##Reference(void) const;              \
  C##type* getDirect##type##Reference(void) const;          \
  C##type* getBase##type##Reference(void) const;            \
  void removeRefInheritance();                              \
  const StdString& getBase##type##Id(void) const;           \
  void solveRefInheritance(bool apply = true);              \
  void solveBaseReference(void);                            \
                                                            \
private:                                                    \
  C##type* baseRefObject;                                   \

// Definitions

#define DEFINE_REF_FUNC(type, name)                                    \
void C##type::solveRefInheritance(bool apply)                          \
{                                                                      \
  std::set<C##type*> refObjects;                                       \
  C##type* refer_ptr = this;                                           \
                                                                       \
  while (refer_ptr->hasDirect##type##Reference())                      \
  {                                                                    \
    refObjects.insert(refer_ptr);                                      \
                                                                       \
    refer_ptr = refer_ptr->getDirect##type##Reference();               \
                                                                       \
    if (refObjects.end() != refObjects.find(refer_ptr))                \
    {                                                                  \
      ERROR("void C" #type "::solveRefInheritance(bool apply)",        \
            << "Circular dependency stopped for " #name " object "     \
            << "with id = \"" << refer_ptr->getId() << "\".");         \
    }                                                                  \
                                                                       \
    SuperClassAttribute::setAttributes(refer_ptr, apply);              \
  }                                                                    \
}                                                                      \
                                                                       \
void C##type::removeRefInheritance()                                   \
{                                                                      \
  if (!this->name##_ref.isEmpty())                                     \
    this->name##_ref.reset();                                          \
}                                                                      \
                                                                       \
void C##type::solveBaseReference(void)                                 \
{                                                                      \
  std::set<C##type*> refObjects;                                       \
  baseRefObject = C##type::get(this);                                  \
                                                                       \
  while (baseRefObject->hasDirect##type##Reference())                  \
  {                                                                    \
    refObjects.insert(baseRefObject);                                  \
                                                                       \
    baseRefObject = baseRefObject->getDirect##type##Reference();       \
                                                                       \
    if (refObjects.end() != refObjects.find(baseRefObject))            \
    {                                                                  \
      ERROR("void C" #type "::solveBaseReference(void)",               \
            << "Circular dependency stopped for " #name " object "     \
            << "with id = \"" << baseRefObject->getId() << "\".");     \
    }                                                                  \
  }                                                                    \
}                                                                      \
                                                                       \
C##type* C##type::getDirect##type##Reference(void) const               \
{                                                                      \
  if (this->name##_ref.isEmpty())                                      \
    return this->getBase##type##Reference();                           \
                                                                       \
  if (!C##type::has(this->name##_ref))                                 \
    ERROR("C" #type "* C" #type "::getDirect" #type "Reference(void)", \
          << this->name##_ref                                          \
          << " refers to an unknown " #name " id.");                   \
                                                                       \
  return C##type::get(this->name##_ref);                               \
}                                                                      \
                                                                       \
C##type* C##type::getBase##type##Reference(void) const                 \
{                                                                      \
  return baseRefObject;                                                \
}                                                                      \
                                                                       \
const StdString& C##type::getBase##type##Id(void) const                \
{                                                                      \
  return this->getBase##type##Reference()->getId();                    \
}                                                                      \
                                                                       \
bool C##type::hasDirect##type##Reference(void) const                   \
{                                                                      \
  return !this->name##_ref.isEmpty();                                  \
}                                                                      \

#endif // __XIOS_DECLARE_REF_FUNC_HPP__
