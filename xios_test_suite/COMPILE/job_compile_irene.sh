#!/bin/bash
######################
## CURIE   TGCC/CEA ##
######################
#MSUB -r XIOS
#MSUB -o client_output.out    # standard output
#MSUB -e client_error.err    #error output
#MSUB -eo
#MSUB -c 48
#MSUB -n 1  # Number of MPI tasks (SPMD case) or cores (MPMD case)
#MSUB -X 
#MSUB -x 
#MSUB -T 1800      # Wall clock limit (seconds)
#MSUB -q skylake           # thin nodes
#MSUB -A gen0826
#MSUB -Q test
#MSUB -m work,scratch

# ------------- Compile job for Irene------------

cd $BRIDGE_MSUB_PWD

cmake .
ctest -V

exit
