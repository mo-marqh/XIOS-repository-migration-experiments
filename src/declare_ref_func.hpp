/*!
   \file declare_ref_func.hpp
   \author Ha NGUYEN
   \date 02 Dec 2014
   \since 02 Dec 2014

   \brief Macro for some functions to solve reference of base object which are used in class Domain, Axis and Field
 */
#ifndef __XIOS_DECLARE_REF_FUNC_HPP__
#define __XIOS_DECLARE_REF_FUNC_HPP__

#define DECLARE_REF_FUNC(type,name)  \
public:                                                             \
 bool hasDirect##type##Reference(void) const;                       \
 C##type * getDirect##type##Reference(void) const;                 \
 C##type * getBase##type##Reference(void) const;                   \
 void addReference(C##type * field);                               \
 const std::vector<C##type *> & getAllReference(void) const;       \
 void removeRefInheritance();                                       \
 const StdString& getBase##type##Id(void) const;                   \
 void solveRefInheritance(bool apply = true);                             \
 void solveBaseReference(void);                                                                    \
 std::vector<C##type *> refObject;                                 \
 C##type * baseRefObject;                                          \

// Defition of some function for solving reference in class Domain, Axis and Field

#define DEFINE_REF_FUNC(type,name) \
void C##type::solveRefInheritance(bool apply)                          \
{                                                                     \
  std::set<C##type *> sset;                                            \
  C##type * refer_sptr;                                                 \
  C##type * refer_ptr = this;                                          \
                                                                      \
  while (refer_ptr->hasDirect##type##Reference())                        \
  {                                                                   \
     refer_sptr = refer_ptr->getDirect##type##Reference();               \
     refer_ptr  = refer_sptr;                                         \
                                                                      \
     if(sset.end() != sset.find(refer_ptr))                           \
     {                                                                \
        DEBUG (<< "Circular dependency stopped for #name object on "  \
               << "\"" + refer_ptr->getId() + "\" !");                \
        break;                                                        \
     }                                                                \
                                                                      \
     SuperClassAttribute::setAttributes(refer_ptr, apply);            \
     sset.insert(refer_ptr);                                          \
  }                                                                   \
}                                                                     \
                                                                      \
void C##type::removeRefInheritance()                                   \
{                                                                     \
 if (this->name##_ref.isEmpty()) return;                              \
 this->clearAttribute("name##_ref");                                  \
}                                                                     \
                                                                      \
void C##type::solveBaseReference(void)                                 \
{                                                                     \
  std::set<C##type *> sset;                                            \
  C##type * refer_sptr;                                                 \
  C##type * refer_ptr = this;                                          \
                                                                      \
  if (this->hasDirect##type##Reference())  baseRefObject = getDirect##type##Reference(); \
  else  baseRefObject = C##type::get(this);                                         \
                                                                                   \
  while (refer_ptr->hasDirect##type##Reference())                                     \
  {                                                                                \
     refer_sptr = refer_ptr->getDirect##type##Reference();                            \
     refer_ptr  = refer_sptr;                                                      \
                                                                                   \
     if(sset.end() != sset.find(refer_ptr))                                        \
     {                                                                             \
        DEBUG (<< "Circular dependency stopped for #name object on "              \
               << "\"" + refer_ptr->getId() + "\" !");                             \
        break;                                                                     \
     }                                                                             \
                                                                                   \
     sset.insert(refer_ptr);                                                       \
  }                                                                                \
  baseRefObject = refer_ptr;                                                      \
                                                                                   \
  if (hasDirect##type##Reference()) baseRefObject->addReference(this) ;               \
}                                                                                  \
                                                                                   \
C##type * C##type::getDirect##type##Reference(void) const                                \
{                                                                                  \
  if (this->name##_ref.isEmpty())                                                   \
     return (this->getBase##type##Reference());                                       \
                                                                                   \
  if (! C##type::has(this->name##_ref.getValue()))                                   \
     ERROR("C##type::getDirect##type##Reference(void)",                                \
           << "[ ref_name = " << this->name##_ref.getValue() << "]"                 \
           << " invalid #name name !");                                            \
                                                                                   \
  return (C##type::get(this->name##_ref.getValue()));                                \
}                                                                                  \
                                                                                   \
C##type * C##type::getBase##type##Reference(void) const                                  \
{                                                                                  \
  return (baseRefObject);                                                          \
}                                                                                  \
                                                                                   \
const std::vector<C##type *>& C##type::getAllReference(void) const                    \
{                                                                                  \
  return (refObject);                                                              \
}                                                                                  \
                                                                                   \
const StdString & C##type::getBase##type##Id(void) const                               \
{                                                                                  \
  return (this->getBase##type##Reference()->getId());                                 \
}                                                                                  \
                                                                                   \
void C##type::addReference(C##type * _##name##_)                                           \
{                                                                                  \
  refObject.push_back(_##name##_) ;                                                     \
}                                                                                   \
                                                                                    \
bool C##type::hasDirect##type##Reference(void) const                                \
{                                                                                   \
 return (!this->name##_ref.isEmpty());                                               \
}                                                                                   \

#endif // __XIOS_DECLARE_REF_FUNC_HPP__
