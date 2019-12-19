#!/bin/bash
##SBATCH --nodes=2
#SBATCH --ntasks=8              # Nombre total de processus MPI
#SBATCH --ntasks-per-node=16
#SBATCH --hint=nomultithread
#SBATCH -t 00:30:00              # Temps elapsed maximum d'exécution
#SBATCH -o output.out     # Nom du fichier de sortie
#SBATCH -e error.err     # Nom du fichier d'erreur
#SBATCH --account=psl@cpu

#----------------- Compile job for Jean-Zay --------------


cd ${SLURM_SUBMIT_DIR}

module load cmake

ulimit -c 0

export machine=jeanzay
export revision=$(svn info --show-item revision .. 2>&1)

python config_compile.py

cmake .
ctest -V

mkdir -p ../html
mkdir -p ../html/build_${machine}

mv build_*.txt ../html/build_${machine}/

bash -c "cd ../html && python generate_compile.py"


