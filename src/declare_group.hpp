#ifndef __XIOS_DECLARE_GROUP__
#define __XIOS_DECLARE_GROUP__

/// ///////////////////////////// Macros ///////////////////////////// ///

#define DECLARE_GROUP(type)                                           \
   class type##Group                                                  \
      : public CGroupTemplate<type, type##Group, type##Attributes>    \
   {                                                                  \
      public:                                                         \
         typedef type              RelChild;                          \
         typedef type##Group       RelGroup;                          \
         typedef type##Attributes  RelAttributes;                     \
                                                                      \
         type##Group(CContext* context)                                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (context)    \
         { /* Ne rien faire de plus */ }                              \
         type##Group(CContext* context, const StdString& _id)                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (context, _id) \
         { /* Ne rien faire de plus */ }                              \
                                                                      \
         static ENodeType GetType(void)                               \
         { return static_cast<ENodeType>(RelChild::GetType()+1); }    \
                                                                      \
         virtual ~type##Group(void)                                   \
         { /* Ne rien faire de plus */ }                              \
   };                                                                 \
   typedef type##Group type##Definition

#define DECLARE_GROUP_CONTEXT(type)                                           \
   class type##Group                                                  \
      : public CGroupTemplate<type, type##Group, type##Attributes>    \
   {                                                                  \
      public:                                                         \
         typedef type              RelChild;                          \
         typedef type##Group       RelGroup;                          \
         typedef type##Attributes  RelAttributes;                     \
                                                                      \
         type##Group()                                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> ()    \
         { /* Ne rien faire de plus */ }                              \
         type##Group(const StdString& _id)                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (_id) \
         { /* Ne rien faire de plus */ }                              \
                                                                      \
         type##Group(CContext* context)                               \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (context) \
         { /* Ne rien faire de plus */ }                              \
                                                                      \
         type##Group(CContext* context, const StdString& _id)                            \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (context, _id) \
         { /* Ne rien faire de plus */ }                              \
                                                                      \
         static ENodeType GetType(void)                               \
         { return static_cast<ENodeType>(RelChild::GetType()+1); }    \
                                                                      \
         virtual ~type##Group(void)                                   \
         { /* Ne rien faire de plus */ }                              \
   };                                                                 \
   typedef type##Group type##Definition


   
#define DECLARE_GROUP_PARSE_REDEF(type)                                  \
   class type##Group                                                     \
      : public CGroupTemplate<type, type##Group, type##Attributes>       \
   {                                                                     \
      public:                                                            \
         typedef type              RelChild;                             \
         typedef type##Group       RelGroup;                             \
         typedef type##Attributes  RelAttributes;                        \
         typedef CGroupTemplate<type, type##Group, type##Attributes>     \
                 SuperClass;                                             \
                                                                         \
         type##Group(CContext* context)                                               \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (context)       \
         { /* Ne rien faire de plus */ }                                 \
         type##Group(CContext* context, const StdString& _id)                               \
            : CGroupTemplate<RelChild, RelGroup, RelAttributes> (context, _id)    \
         { /* Ne rien faire de plus */ }                                 \
                                                                         \
         static ENodeType GetType(void)                                  \
         { return static_cast<ENodeType>(RelChild::GetType()+1); }       \
                                                                         \
         virtual void parse(xml::CXMLNode & node, bool withAttr = true); \
                                                                         \
         virtual ~type##Group(void)                                      \
         { /* Ne rien faire de plus */ }                                 \
   };                                                                    \
   typedef type##Group type##Definition

#endif // __XIOS_DECLARE_GROUP__
