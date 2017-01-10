#!/bin/bash 

#MSUB -r test_client # Request name 
#MSUB -n 8              # Number of tasks to use 
##MSUB -c 1              # Number of threads to use 
#MSUB -T 1800            # Elapsed time limit in seconds of the job (default: 1800) 
#MSUB -o client_output.out      # Standard output. %I is the job id
#MSUB -e client_error.err       # Error output. %I is the job id
#MSUB -A gen0826         # Project ID 2211
#MSUB -Q test            # Qos
#MSUB -q standard        # Chosing standard node
#MSUB -X 

CURRENT=$PWD
cd $CURRENT/inputs
rm -f output_?.*
rm -f output.*
rm -f output_*
rm -f *.err
rm -f *.out
rm -f info_output_*
rm -f *.nc
rm -f appClient.conf_*

ulimit -a
ulimit -aH
#module load ipm

#totalview
#ccc_mprun -d tv ../bin/test_client.exe  # debug with total view
#ccc_mprun ../bin/test_client.exe
#totalview ccc_mprun -a -np 6 ../bin/test_client.exe # : -np 6 ../bin/xios_server.exe"

#ccc_mprun  amplxe-gui 
#ccc_mprun -f appClient.conf
#ddt -start -n 7 -mpiargs "-np 6 ../bin/test_client.exe : -np 1 ../bin/xios_server.exe" 
ddt -start -n 6 -mpiargs "-np 2 ../bin/test_client.exe : -np 2 ../bin/xios_server1.exe : -np 2 ../bin/xios_server2.exe" 

#module load map
#map -profile -n 1 ../bin/test_client.exe
#ccc_mprun -d vtune -o collect_hotspots -t hotspots ../bin/test_client.exe

#module load valgrind
#export LD_PRELOAD=$VALGRIND_LIBDIR/valgrind/libmpiwrap-amd64-linux.so
#rm -f gdb-*.*
#rm -f valgrind_*
#ccc_mprun valgrind --log-file=valgrind_%q{SLURM_JOBID}_%q{SLURM_PROCID} --track-origins=yes --leak-check=full ../bin/test_client.exe
