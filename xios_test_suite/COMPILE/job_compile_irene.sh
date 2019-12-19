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
#MSUB -m work

# ------------- Compile job for Irene------------

cd $BRIDGE_MSUB_PWD

set -x 

export revision=$(svn info --show-item revision .. 2>&1)

python config_compile.py

cmake .
ctest -V

build_dir=${xios_test_suite_repository}/BUILD
mkdir -p $build_dir
chmod --quiet ug+rwX $build_dir
mkdir -p ${build_dir}/build_${xios_machine_name}

cp build_*.txt ${build_dir}/build_${xios_machine_name}
chmod -R ug+rwX ${build_dir}/build_${xios_machine_name}

bash -c "python ./generate_compile.py"

copy_to_thredds compile_${xios_machine_name}_info.js

exit
