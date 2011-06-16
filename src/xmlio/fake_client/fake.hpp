#ifndef __XMLIO_FAKE__
#define __XMLIO_FAKE__

#ifdef __cplusplus
extern"C"
{
#endif //__cplusplus

   // Point d'entrée du faux client nemo (src/fake_client/fake_nemo.f90)
   void nemo_fake_entry(MPIComm comm_client,
                        MPIComm comm_client_grp,
                        MPIComm comm_client_server);

   // Point d'entrée du faux client nemo (src/fake_client/fake_lmdz.f90)
   void lmdz_fake_entry(MPIComm comm_client,
                        MPIComm comm_client_grp,
                        MPIComm comm_client_server);

   // Point d'entrée du faux client nemo (src/fake_client/fake_orchidee.f90)
   void orchidee_fake_entry(MPIComm comm_client,
                            MPIComm comm_client_grp,
                            MPIComm comm_client_server);


#ifdef __cplusplus
}
#endif //__cplusplus

#endif //__XMLIO_FAKE__
