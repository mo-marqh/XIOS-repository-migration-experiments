#!/bin/bash
######################
## CURIE   TGCC/CEA ##
######################
#MSUB -r XIOS
#MSUB -o client_output.out    # standard output
#MSUB -e client_error.err    #error output
#MSUB -eo
#MSUB -c 1
#MSUB -n 16  # Number of MPI tasks (SPMD case) or cores (MPMD case)
#MSUB -X 
#MSUB -x 
#MSUB -T 1800      # Wall clock limit (seconds)
#MSUB -q skylake           # thin nodes
#MSUB -A devcmip6
##MSUB -A gen0826
#MSUB -Q test
#MSUB -m work

# ------------- Compile job for Irene------------

cd $BRIDGE_MSUB_PWD
module load subversion
module load cmake

export KMP_STACKSIZE=3g
export KMP_LIBRARY=turnaround
export MKL_SERIAL=YES
export OMP_NUM_THREADS=${BRIDGE_MSUB_NCORE}

set -x 

export machine=irene
export revision=$(svn info --show-item revision .. 2>&1)

python config_compile.py

cmake .
ctest -V


mkdir -p ../html
mkdir -p ../html/build_${machine}

mv build_*.txt ../html/build_${machine}/
bash -c "cd ../html && python generate_compile.py"

exit
