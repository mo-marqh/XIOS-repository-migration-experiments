#!/bin/bash

cp ../context_grid_dynamico.xml ./
cp ../dynamico_grid.nc ./
cp ../config.py sub_config.py
cp ../check.py sub_check.py
cp ../default_param.py default_param.py


{ nb_proc=$(python sub_config.py 2>&1 1>&3-) ;} 3>&1

if [[ ($machine_name == irene) ]]
then
    echo "Testing on Irene"
    echo "build_dir="$build_dir
    ccc_mprun -n $nb_proc $xios_dir/$build_dir/bin/generic_testcase.exe
elif [[($machine_name == jeanzay)]]
then
    echo "Testing on Jean-Zay" 
    echo "build_dir="$build_dir
    srun -n $nb_proc --mpi=pmi2 $xios_dir/$build_dir/bin/generic_testcase.exe
    if [ $? -ne 0 ]
        then
            echo "execution failed"
            rm -f iodef.xml
            rm -f user_param.py
            rm -f default_param.py
            rm -f sub_config.py
            rm -f sub_check.py
            exit 1234
    fi
else
    echo "other machine"
    exit 1234
fi


{ check_stderr=$(python sub_check.py 2>&1 1>&3-) ;} 3>&1  #captures stderr, letting stdout through

if [[ "$check_stderr" -ne 0 ]]
then
    echo "check.py failed"
    rm -f iodef.xml
    rm -f user_param.py
    rm -f default_param.py
    rm -f sub_config.py
    rm -f sub_check.py
    exit 1234
fi


rm -f iodef.xml
rm -f user_param.py
rm -f default_param.py
rm -f sub_config.py
rm -f sub_check.py
