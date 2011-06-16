#ifndef __XMLIO_DECLARE_ATTRIBUTE__
#define __XMLIO_DECLARE_ATTRIBUTE__

/// ///////////////////////////// Macros ///////////////////////////// ///

#define DECLARE_ATTRIBUTE(type, name)                             \
   class name##att : public CAttributeTemplate<type>{             \
      public :                                                    \
         name##att(void)                                          \
            : CAttributeTemplate<type>                            \
            (#name, *CAttributeMap::Current)                      \
         { /* Ne rien faire de plus */ }                          \
         type operator=(const type & value)                       \
         { return (CAttributeTemplate<type>::operator=(value)); } \
         virtual ~name##att(void)                                 \
         { /* Ne rien faire de plus */ }                          \
   } name;

#define DECLARE_ATTRIBUTE_DEF(type, name, def)                     \
   class name##att : public CAttributeTemplate<type>{              \
      public :                                                     \
         name##att(void)                                           \
            : CAttributeTemplate<type>                             \
            (#name, def, *CAttributeMap::Current)                  \
         { /* Ne rien faire de plus */ }                           \
         type operator=(const type & value)                        \
         { return (CAttributeTemplate<type>::operator=(value)); }  \
         virtual ~name##att(void)                                  \
         { /* Ne rien faire de plus */ }                           \
   } name;

#define BEGIN_DECLARE_ATTRIBUTE_MAP(type)                  \
   class type##Attributes : public virtual CAttributeMap { \
      public :

#define END_DECLARE_ATTRIBUTE_MAP(type)            \
         type##Attributes (void) : CAttributeMap() \
         { /* Ne rien faire de plus */ }           \
         virtual ~type##Attributes (void)          \
         { /* Ne rien faire de plus */ }           \
   };

#endif // __XMLIO_DECLARE_ATTRIBUTE__
