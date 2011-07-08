#ifndef __MPI_INTERFACE__
#define __MPI_INTERFACE

#ifdef __cplusplus
extern"C"
{
#endif //__cplusplus

/// Groupe ///
extern int mpi_comm_world;
extern int mpi_success;
extern int mpi_char;
extern int mpi_status_size;
extern int mpi_any_tag;
extern int mpi_request_null;


/// Initialisation / Finalisation ///
void mpi_init(int * err);
void mpi_finalize(int * err);

/// Quitter brutalement MPI ///
void mpi_abort(int * comm, int * errcode, int * err);

/// Savoir si un processus à fait un MPI_INIT ///
void mpi_initialized(bool * flag, int * err);

/// Récupérer la chaine de caractères associée au code d'erreur err(non fonctionnelle) ///
void mpi_error_string(int * errcode, char chaine[], int * taille_chaine, int * err);

/// Envoyer un message à un processus ///
void mpi_send(char buf[], int * count, int * datatype, int * dest, int * tag, int * comm, int * err);

/// Recevoir un message d'un processus ///
void mpi_recv(char buf[], int * count, int * datatype, int * source,  int * tag, int * comm, int status[], int * err);

/// Envoyer et recevoir un message ///
void mpi_sendrecv(char sendbuf[], int * sendcount, int * sendtype, int * dest, int * sendtag, int recvbuf[], int * recvcount,
                  int * recvtype, char source[], int * recvtag, int * comm, int status[], int * err) ;

/// Compter le nombre d'éléments reçus ///
void mpi_get_count(int status[], int * datatype, int * count, int * err);

/// Tester l'arrivée d'un message ///
void mpi_iprobe(int * source, int * tag, int * comm, bool * flag, int status[], int * err);

/// Nombre de processus dans un intracommunicateur ///
void mpi_comm_size(int * comm, int * nbre, int * err);

/// Rang d'un processus dans un intracommunicateur ///
void mpi_comm_rank(int * comm, int * rang, int * err);

/// Partage d'un communicateur ///
void mpi_comm_split(int * comm, int * couleur, int * cle, int * newcomm, int * err);

/// Commencer à envoyer un message ///
void mpi_issend(char * buf, int * count, int * datatype, int * dest,
                int * tag, int * comm, int * request, int * err);

/// Commencer à recevoir un message ///
void mpi_irecv(char buf[], int * count, int * datatype, int * source,
                            int * tag, int * comm, int * request, int * err);

/// Compléter une opération non bloquante ///
void mpi_wait(int * request, int status[], int * err);

/// Tester une opération non bloquante ///
void mpi_test(int * request, bool * flag, int status[], int * err);

/// Création d'un communicateur à partir d'un groupe ///
void mpi_comm_create(int * comm, int * group, int * newcomm, int * err);

/// Obtention d'un groupe à partir d'un communicateur ///
void mpi_comm_group(int * comm, int * group, int * err);

/// Création de sous-groupe ///
void mpi_group_incl(int * group, int * n, const int rank[], int * newgroup, int * err);

/// Barrière ///
void mpi_barrier(int * comm, int * err);

/// Collecte de données et rediffusion ///
void mpi_allgather(int sendbuf[], int * sendcount, int recvbuf[], int * recvcount, int * comm, int * ierror);

#ifdef __cplusplus
}
#endif //__cplusplus

#endif //__MPI_INTERFACE__
