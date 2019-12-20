#!/bin/bash
#SBATCH --ntasks=1              # Nombre total de processus MPI
#SBATCH --cpu-per-task=40
#SBATCH --hint=nomultithread
#SBATCH -t 00:30:00              # Temps elapsed maximum d'exécution
#SBATCH -o output.out     # Nom du fichier de sortie
#SBATCH -e error.err     # Nom du fichier d'erreur
#SBATCH -A psl@cpu

#----------------- Compile job for Jean-Zay --------------


cd ${SLURM_SUBMIT_DIR}

cmake .
ctest -V

exit
