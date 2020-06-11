#!/bin/bash
##SBATCH --nodes=2
#SBATCH --ntasks=32              # Nombre total de processus MPI
##SBATCH --ntasks-per-node=16
##SBATCH --cpus-per-task=1
#SBATCH --hint=nomultithread
#SBATCH -t 00:10:00              # Temps elapsed maximum d'exécution
#SBATCH -o output.out     # Nom du fichier de sortie
#SBATCH -e error.err     # Nom du fichier d'erreur
##SBATCH -p cpu_port             # Nom de la partition d'exécution
##SBATCH -A sos@cpu
##SBATCH --distribution=arbitrary
#SBATCH --account=psl@cpu

ulimit -c 0

cd ${SLURM_SUBMIT_DIR}

#============================= X64_JEANZAY_prod =============================

export arch=X64_JEANZAY
export mode=prod

time ./run_test


#============================= X64_JEANZAY_debug =============================

export arch=X64_JEANZAY
export mode=debug

time ./run_test
