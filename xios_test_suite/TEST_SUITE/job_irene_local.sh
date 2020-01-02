#!/bin/bash
######################
## CURIE   TGCC/CEA ##
######################
#MSUB -r XIOS
#MSUB -o client_output.out    # standard output
#MSUB -e client_error.err    #error output
#MSUB -eo
#MSUB -c 1
#MSUB -n 48  # Number of MPI tasks (SPMD case) or cores (MPMD case)
#MSUB -X 
#MSUB -x 
#MSUB -T 1800      # Wall clock limit (seconds)
#MSUB -q skylake           # thin nodes
#MSUB -A gen0826
#MSUB -Q test
#MSUB -m work,scratch


#============================= X64_IRENE_prod =============================


cd $BRIDGE_MSUB_PWD

export arch=X64_IRENE
export mode=prod

./run_test_local


#============================= X64_IRENE_debug =============================


cd $BRIDGE_MSUB_PWD

export arch=X64_IRENE
export mode=debug

./run_test_local




