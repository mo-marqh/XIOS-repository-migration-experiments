#!/bin/bash
##SBATCH --nodes=2
#SBATCH --ntasks=8              # Nombre total de processus MPI
#SBATCH --ntasks-per-node=16
#SBATCH --hint=nomultithread
#SBATCH -t 00:30:00              # Temps elapsed maximum d'exécution
#SBATCH -o output.out     # Nom du fichier de sortie
#SBATCH -e error.err     # Nom du fichier d'erreur
##SBATCH --account=psl@cpu
#SBATCH -A psl@cpu

#----------------- Compile job for Jean-Zay --------------


cd ${SLURM_SUBMIT_DIR}

cmake .
ctest -V

exit
