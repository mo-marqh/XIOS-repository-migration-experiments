/* ************************************************************************** *
 *      Copyright © IPSL/LSCE, xios, Avril 2010 - Octobre 2011         *
 * ************************************************************************** */

#include <boost/multi_array.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <iostream>


#include "xmlioserver.hpp"
#include "oasis_cinterface.hpp"

#include "attribute_template_impl.hpp"
#include "object_template_impl.hpp"
#include "group_template_impl.hpp"

#include "icutil.hpp"
#include "cxios.hpp"
#include "client.hpp"
#include "field.hpp"
#include "field_impl.hpp"
#include <mpi.h>
#include "timer.hpp"

extern "C"
{
// /////////////////////////////// Définitions ////////////////////////////// //

   // ----------------------- Redéfinition de types ----------------------------
   
   typedef enum { NETCDF4 = 0 } XFileType;
   
   typedef xios::CContext * XContextPtr;

   // -------------------- Traitement des données ------------------------------
   void cxios_init_server(void)
   {
     CTimer::get("XIOS").resume() ;
     CXios::initServerSide();      
     CTimer::get("XIOS").suspend() ;
   }

   void cxios_init_client(const char * client_id , int len_client_id, MPI_Fint* f_local_comm, MPI_Fint* f_return_comm )
   {
      std::string str; 
      MPI_Comm local_comm ;
      MPI_Comm return_comm ;
      
      if (!cstr2string(client_id, len_client_id, str)) return;

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS init").resume() ;
      int initialized ;
      MPI_Initialized(&initialized) ;
      if (initialized) local_comm=MPI_Comm_f2c(*f_local_comm) ;
      else local_comm=MPI_COMM_NULL ;
      CXios::initClientSide(str,local_comm,return_comm);
      *f_return_comm=MPI_Comm_c2f(return_comm) ;
      CTimer::get("XIOS init").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }

   void cxios_context_initialize(const char * context_id , int len_context_id, MPI_Fint* f_comm)
   {
     std::string str; 
     MPI_Comm comm ;
     
     if (!cstr2string(context_id, len_context_id, str)) return;
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS init context").resume() ;
     comm=MPI_Comm_f2c(*f_comm) ;
     CClient::registerContext(str,comm) ;
     CTimer::get("XIOS init context").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }
 
    void cxios_context_close_definition()
   {
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS close definition").resume() ;
     CContext* context = CContext::getCurrent() ;
     context->closeDefinition() ;
     CTimer::get("XIOS close definition").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }  

   void cxios_context_finalize()
   {
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS context finalize").resume() ;
     CContext* context = CContext::getCurrent() ;
     context->finalize() ;
     CTimer::get("XIOS context finalize").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_finalize()
   {
     CTimer::get("XIOS").resume() ;
     CTimer::get("XIOS finalize").resume() ;
     CXios::clientFinalize() ;
     CTimer::get("XIOS finalize").suspend() ;
     CTimer::get("XIOS").suspend() ;
   }

 
   
   // ---------------------- Ecriture des données ------------------------------
   
   void cxios_write_data_k81(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 
      
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
//      boost::const_multi_array_ref<double, 1> array_(data_k8,
//          boost::extents [data_Xsize],
//          boost::fortran_storage_order());
      ARRAY(double, 1) data(new CArray<double, 1>(boost::extents [data_Xsize]));
      std::copy(data_k8, &(data_k8[data->num_elements()]), data->data());
      
//      dtreat->write_data(fieldid_str, data);
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k82(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize, int data_Ysize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return;
      
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
      
//      boost::const_multi_array_ref<double, 2> array_(data_k8,
//          boost::extents [data_Xsize][data_Ysize],
//          boost::fortran_storage_order());
      ARRAY(double, 2) data(new CArray<double, 2>(boost::extents [data_Xsize][data_Ysize]));
      std::copy(data_k8, &(data_k8[data->num_elements()]), data->data());
//      dtreat->write_data(fieldid_str, data);
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k83(const char * fieldid, int fieldid_size, double * data_k8, int data_Xsize, int data_Ysize, int data_Zsize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
//      boost::const_multi_array_ref<double, 3> array_(data_k8,
//          boost::extents [data_Xsize][data_Ysize][data_Zsize],
//          boost::fortran_storage_order());
      ARRAY(double, 3) data(new CArray<double, 3>(boost::extents [data_Xsize][data_Ysize][data_Zsize]));
      std::copy(data_k8, &(data_k8[data->num_elements()]), data->data());
//      dtreat->write_data(fieldid_str, data);
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k41(const char * fieldid, int fieldid_size, float * data_k4, int data_Xsize)
   {
      std::string fieldid_str;
     if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
//      boost::const_multi_array_ref<float, 1> array_(data_k4,
//          boost::extents [data_Xsize],
//          boost::fortran_storage_order());
//      ARRAY(float, 1) data(new CArray<float, 1>(boost::extents [data_Xsize]));
//      std::copy(data_k4, &(data_k4[data->num_elements()]), data->data());
//      dtreat->write_data(fieldid_str, data);
      ARRAY(double, 1) data(new CArray<double, 1>(boost::extents [data_Xsize]));
      double* ptr_data=data->data() ; 
      for(int i=0;i<data->num_elements();i++) ptr_data[i]=data_k4[i];
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k42(const char * fieldid, int fieldid_size, float * data_k4, int data_Xsize, int data_Ysize)
   {
      std::string fieldid_str;
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 

      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
//      boost::const_multi_array_ref<float, 2> array_(data_k4,
//          boost::extents [data_Xsize][data_Ysize],
//          boost::fortran_storage_order());
//      ARRAY(float, 2) data(new CArray<float, 2>(boost::extents [data_Xsize][data_Ysize]));
//      std::copy(data_k4, &(data_k4[data->num_elements()]), data->data());
//      dtreat->write_data(fieldid_str, data);
      ARRAY(double, 2) data(new CArray<double, 2>(boost::extents [data_Xsize][data_Ysize]));
      double* ptr_data=data->data() ; 
      for(int i=0;i<data->num_elements();i++) ptr_data[i]=data_k4[i];
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;
   }
   
   void cxios_write_data_k43(const char * fieldid, int fieldid_size, float * data_k4, int data_Xsize, int data_Ysize, int data_Zsize)
   {
      std::string fieldid_str;
 
      if (!cstr2string(fieldid, fieldid_size, fieldid_str)) return; 
 
      CTimer::get("XIOS").resume() ;
      CTimer::get("XIOS send field").resume() ;
      CContext* context = CContext::getCurrent() ;
     
//      boost::const_multi_array_ref<float, 3> array_(data_k4,
//          boost::extents [data_Xsize][data_Ysize][data_Zsize],
//          boost::fortran_storage_order());
//      ARRAY(float, 3) data(new CArray<float, 3>(boost::extents [data_Xsize][data_Ysize][data_Zsize]));
//      std::copy(data_k4, &(data_k4[data->num_elements()]), data->data());
//      dtreat->write_data(fieldid_str, data);
      ARRAY(double, 3) data(new CArray<double, 3>(boost::extents [data_Xsize][data_Ysize][data_Zsize]));
      double* ptr_data=data->data() ; 
      for(int i=0;i<data->num_elements();i++) ptr_data[i]=data_k4[i];
      CField::get(fieldid_str)->setData(data) ;
      CTimer::get("XIOS send field").suspend() ;
      CTimer::get("XIOS").suspend() ;

    } 

} // extern "C"
