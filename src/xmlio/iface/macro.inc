#define MAKE_STRING(var_name, str_arg, int_arg) ;;     \
   StdString var_name##temp; ;;                        \
   StdSize d, f = 0; ;;                                \
   if (int_arg != -1) ;;                               \
   { ;;                                                \
      var_name##temp.append (str_arg, int_arg); ;;     \
      d = var_name##temp.find_first_not_of(' '); ;;    \
      f = var_name##temp.find_last_not_of (' '); ;;    \
   } ;;                                                \
   StdString var_name = (int_arg != -1)                \
                     ? var_name##temp.substr(d, f-d+1) \
                     : "" ; ;

#define MAKE_ARRAY(value, valuetype, numdims, extent)            \
   boost::shared_ptr<CArray<valuetype, numdims> > value##_val ;; \
   (new CArray<valuetype, numdims>(boost::extents extent));   ;; \
   std::copy(value, &(value[value##_val->num_elements()]), value##_val->data()); ;


//-------------------------------

#define iarg_bool(name)      , const bool   * name
#define iarg_int(name)       , const int    * name
#define iarg_double(name)    , const double * name
#define iarg_StdString(name) , const char   * name iarg_int(name##_size)

#define iarg_ARRAY1(name) name iarg_int(name##_extent1)
#define iarg_ARRAY2(name) iarg_ARRAY1(name) iarg_int(name##_extent2)
#define iarg_ARRAY3(name) iarg_ARRAY2(name) iarg_int(name##_extent3)
#define iarg_ARRAY(type, numdim) , const type *  iarg_ARRAY##numdim

//--------------------------------
#define dec_bool(name)      bool   name##_val = *name;
#define dec_int(name)       int    name##_val = *name;
#define dec_double(name)    double name##_val = *name;
#define dec_StdString(name) MAKE_STRING(name##_val, name, *name##_size);

//-

#define dec_double1(name)     \
   dec_int(name##_extent1) ;; \
   MAKE_ARRAY(name, double, 1, [name##_extent1##_val]);

#define dec_double2(name)     \
   dec_int(name##_extent1) ;; \
   dec_int(name##_extent2) ;; \
   MAKE_ARRAY(name, double, 2, [name##_extent1##_val][name##_extent2##_val]);

#define dec_double3(name)     \
   dec_int(name##_extent1) ;; \
   dec_int(name##_extent2) ;; \
   dec_int(name##_extent3) ;; \
   MAKE_ARRAY(name, double, 3, [name##_extent1##_val][name##_extent2##_val])[name##_extent3##_val]);

//-

#define dec_int1(name)        \
   dec_int(name##_extent1) ;; \
   MAKE_ARRAY(name, int, 1, [name##_extent1##_val]);

#define dec_int2(name)        \
   dec_int(name##_extent1) ;; \
   dec_int(name##_extent2) ;; \
   MAKE_ARRAY(name, int, 2, [name##_extent1##_val][name##_extent2##_val]);

#define dec_int3(name)        \
   dec_int(name##_extent1) ;; \
   dec_int(name##_extent2) ;; \
   dec_int(name##_extent3) ;; \
   MAKE_ARRAY(name, int, 3, [name##_extent1##_val][name##_extent2##_val])[name##_extent3##_val]);

//-

#define dec_bool1(name)       \
   dec_int(name##_extent1) ;; \
   MAKE_ARRAY(name, bool, 1, [name##_extent1##_val]);

#define dec_bool2(name)       \
   dec_int(name##_extent1) ;; \
   dec_int(name##_extent2) ;; \
   MAKE_ARRAY(name, bool, 2, [name##_extent1##_val][name##_extent2##_val]);

#define dec_bool3(name)       \
   dec_int(name##_extent1) ;; \
   dec_int(name##_extent2) ;; \
   dec_int(name##_extent3) ;; \
   MAKE_ARRAY(name, bool, 3, [name##_extent1##_val][name##_extent2##_val])[name##_extent3##_val]);

//-

#define dec_ARRAY(type, numdim) dec_##type##numdim

//--------------------------------

#define DECLARE_INTERFACE(class, type, name);;                    \
   void xios_set_##class##_##name                                 \
      (void * class##_hdl, int ftype iarg_##type(name));;         \
   {;;                                                            \
      dec_##type(name) ;;                                         \
      if (class##_hdl != NULL) ;;                                 \
      {;;                                                         \
         if (ftype < GAXIS);;                                     \
         {;;                                                      \
            c##class * class##_hdl_ = (c##class *) class##_hdl; ;;\
            class##_hdl_->name.setValue(name##_val); ;;           \
         };;                                                      \
         else;;                                                   \
         {;;                                                      \
            c##class##group * class##_hdl_;;                      \
              = (c##class##group *) class##_hdl; ;;               \
            class##_hdl_->name.setValue(name##_val); ;;           \
         };;                                                      \
      };;                                                         \
   };; ;;
