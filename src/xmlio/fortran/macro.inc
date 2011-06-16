#define iarg_bool(name) ,name
#define iarg_double(name) ,name
#define iarg_int(name) ,name
#define iarg_StdString(name) ,name iarg_int(name##_size)

#define iarg_ARRAY1(name) ,name iarg_int(name##_extent1)
#define iarg_ARRAY2(name) iarg_ARRAY1(name) iarg_int(name##_extent2)
#define iarg_ARRAY3(name) iarg_ARRAY2(name) iarg_int(name##_extent3)
#define iarg_ARRAY(type, numdim) iarg_ARRAY##numdim

//--------------------------------

#define arg_bool(name) ,name##_
#define arg_double(name) ,name##_
#define arg_int(name) ,name##_
#define arg_StdString(name) ,name##_
#define arg_ARRAY_(name) ,name##_
#define arg_ARRAY(type, numdim) arg##_ARRAY_

//---------------------------------

#define dec_bool(name) \
   LOGICAL (kind = C_BOOL) :: name;

#define dec_int(name) \
   INTEGER (kind = C_INT) :: name;
   
#define dec_double(name) \
   REAL (kind = C_DOUBLE) :: name;

#define dec_StdString(name) \
   CHARACTER(kind = C_CHAR), DIMENSION(*) :: name; \
   dec_int(name##_size)

#define dec_double1(name) \
   REAL(kind = C_DOUBLE), DIMENSION(*) :: name; \
   dec_int(name##_extent1)
#define dec_double2(name) dec_double1(name) dec_int(name##_extent2)
#define dec_double3(name) dec_double2(name) dec_int(name##_extent3)

#define dec_bool1(name) \
   LOGICAL(kind = C_BOOL), DIMENSION(*) :: name; \
   dec_int(name##_extent1)
#define dec_bool2(name) dec_bool1(name) dec_int(name##_extent2)
#define dec_bool3(name) dec_bool2(name) dec_int(name##_extent3)

#define dec_int1(name) \
   INTEGER(kind = C_INT), DIMENSION(*) :: name; \
   dec_int(name##_extent1)
#define dec_int2(name) dec_int1(name) dec_int(name##_extent2)
#define dec_int3(name) dec_int2(name) dec_int(name##_extent3)

#define dec_ARRAY(type, numdim) dec_##type##numdim

//---------------------------------


#define def_bool(name) \
   LOGICAL(kind = 1),  OPTIONAL, INTENT(IN) :: name##_

#define def_int(name) \
   INTEGER,  OPTIONAL, INTENT(IN) :: name##_
   
#define def_double(name) \
   REAL(kind=8),  OPTIONAL, INTENT(IN) :: name##_

#define def_StdString(name) \
   CHARACTER(len = *), OPTIONAL, INTENT(IN) :: name##_


#define def_double1(name) \
   REAL(kind = 8), dimension(*), OPTIONAL, INTENT(IN) :: name##_(:)
#define def_double2(name) \
   REAL(kind = 8), dimension(*), OPTIONAL, INTENT(IN) :: name##_(:,:)
#define def_double3(name) \
   REAL(kind = 8), dimension(*), OPTIONAL, INTENT(IN) :: name##_(:,:,:)

//-

#define def_bool1(name) \
   LOGICAL(kind = 1), dimension(*), OPTIONAL, INTENT(IN) :: name##_(:)
#define def_bool2(name) \
   LOGICAL(kind = 1), dimension(*), OPTIONAL, INTENT(IN) :: name##_(:,:)
#define def_bool3(name) \
   LOGICAL(kind = 1), dimension(*), OPTIONAL, INTENT(IN) :: name##_(:,:,:)

//-

#define def_int1(name) \
   INTEGER, dimension(*), OPTIONAL, INTENT(IN) :: name##_(:)
#define def_int2(name) \
   INTEGER, dimension(*), OPTIONAL, INTENT(IN) :: name##_(:,:)
#define def_int3(name) \
   INTEGER, dimension(*), OPTIONAL, INTENT(IN) :: name##_(:,:,:)

#define def_ARRAY(type, numdim) def_##type##numdim

//---------------------------------

#define ip_simple(class, name) \
   IF (PRESENT(name##_)) THEN; \
      CALL xios_set_##class##_##name(class##_hdl%daddr, ftype, name##_); \
   END IF;

#define ip_bool(class, name) ip_simple(class, name)
#define ip_int(class, name) ip_simple(class, name)
#define ip_double(class, name) ip_simple(class, name)

#define ip_StdString(class, name) \
   IF (PRESENT(name##_)) THEN; \
      CALL xios_set_##class##_##name(class##_hdl%daddr, ftype, name##_, len(name##_)); \
   END IF;

#define ip_ARRAY1(class, name) \
   IF (PRESENT(name##_)) THEN; \
      CALL xios_set_##class##_##name(class##_hdl%daddr, ftype, name##_, size(name##_, 1)); \
   END IF;

#define ip_ARRAY2(class, name) \
   IF (PRESENT(name##_)) THEN; \
      CALL xios_set_##class##_##name(class##_hdl%daddr, ftype, name##_, size(name##_, 1), size(name##_, 2)); \
   END IF;

#define ip_ARRAY3(class, name) \
    IF (PRESENT(name##_)) THEN; \
      CALL xios_set_##class##_##name(class##_hdl%daddr, ftype, name##_, size(name##_, 1), size(name##_, 3), size(name##_, 3)); \
   END IF;

#define ip_ARRAY(type, numdim) ip_ARRAY##numdim

//---------------------------------

#define  DECLARE_INTERFACE(class, type, name)                                           \
   SUBROUTINE xios_set_##class##_##name(class##_hdl, ftype iarg_##type(name)) BIND(C);  \
   USE ISO_C_BINDING;                                                                   \
   INTEGER  (kind = C_INTPTR_T), VALUE :: class##_hdl;                                  \
   INTEGER  (kind = C_INT), VALUE      :: ftype;                                        \
   dec_##type(name)                                                                     \
   END SUBROUTINE xios_set_##class##_##name;                                            \
//   SUBROUTINE xios_get_##class##_##name(class##_hdl iarg_##type(name)) BIND(C); \
//   USE ISO_C_BINDING;                                                           \
//   INTEGER  (kind = C_INTPTR_T), VALUE :: class##_hdl;                          \
//   dec_##type(name)                                                             \
//   END SUBROUTINE xios_get_##class##_##name

