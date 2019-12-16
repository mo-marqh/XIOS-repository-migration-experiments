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
#MSUB -Q test
#MSUB -m work


#============================= X64_IRENE_prod =============================


cd $BRIDGE_MSUB_PWD

export arch=X64_IRENE
export mode=prod
export machine_name=irene
export machine_full_name=Irene
export xios_dir=/ccc/cont003/home/gencmip6/wangyush/XIOS/trunk
export reference_dir=${CCCWORKDIR}
export reference_file=xios_reference.tar.gz
export reference_folder=xios_reference

export build_dir=build_${arch}_${mode}
export xios_revision=$(svn info --show-item revision .. 2>&1)
export relurl=$(svn info --show-item relative-url .. 2>&1)

source ../arch/arch-${arch}.env
module load nco
module load cdo
module load subversion

export KMP_STACKSIZE=3g
export KMP_LIBRARY=turnaround
export MKL_SERIAL=YES
export OMP_NUM_THREADS=${BRIDGE_MSUB_NCORE}

set -x 


bash -c "cd .. && ./make_xios --arch ${arch} --${mode} --build_dir ${build_dir} --job 4"

#----------- Fetch reference -----------
cp ${reference_dir}/${reference_file} ./
tar -xzvf ${reference_file}

rm -f ${reference_file}

for i in $(ls ${reference_folder}/) 
do
  folder=${i%%}
  bash -c "cd ${folder} && rm -rf config_*"
  bash -c "cp -r ${reference_folder}/${folder}/config_* ./${folder}"
done

rm -rf ${reference_folder}


rm -f plein_report.txt

echo "#revision" ${xios_revision} >> plein_report.txt
echo "#relurl" ${relurl} >> plein_report.txt
echo "#machine" ${machine_name} >> plein_report.txt
echo "#build_dir" ${xios_dir}/${build_dir} >> plein_report.txt
echo "#arch" ${arch} >> plein_report.txt
echo "#mode" ${mode} >> plein_report.txt


rm -f setup.sh
touch setup.sh
>setup.sh
for i in $(ls -d test_*/)
do
  cp setup.py ${i%%/}      
  cp run_sub_test.sh ${i%%/}    
  echo "bash -c \"cd " ${i%%/} " && python setup.py\" ">> setup.sh
  echo "echo \"setup.py called from " ${i%%/} "\"">> setup.sh
done

export output=$(python user_config.py 2>&1 >/dev/null)

if [ "$output" -ne 0 ]
then
	echo "user_config.py failed"
	exit
else
	echo "user_config.py OK"
fi


cmake .
ctest -V 
#ctest --output-on-failure
make report

rm -f test_*/setup.py
rm -f test_*/run_sub_test.sh
rm -f test_*/run_test_*.py
rm -f test_*/CMakeLists.txt
rm -f test_*/context_grid_dynamico.xml
rm -f test_*/dynamico_grid.nc
rm -f test_*/default_param.pyc
rm -f test_*/user_param.pyc
rm -f test_*/user_param.py.*

mkdir -p ../html
mkdir -p ../html/test_irene

cp plein_report.txt ../html/test_irene/test_${xios_revision}_${machine_name}_${arch}_${mode}.txt

bash -c "cd ../html && python generate_test.py"






#============================= X64_IRENE_debug =============================


cd $BRIDGE_MSUB_PWD

export arch=X64_IRENE
export mode=debug
export machine_name=irene
export machine_full_name=Irene
export xios_dir=/ccc/cont003/home/gencmip6/wangyush/XIOS/trunk
export reference_dir=${CCCWORKDIR}
export reference_file=xios_reference.tar.gz
export reference_folder=xios_reference

export build_dir=build_${arch}_${mode}
export xios_revision=$(svn info --show-item revision .. 2>&1)
export relurl=$(svn info --show-item relative-url .. 2>&1)

source ../arch/arch-${arch}.env
module load nco
module load cdo
module load subversion

export KMP_STACKSIZE=3g
export KMP_LIBRARY=turnaround
export MKL_SERIAL=YES
export OMP_NUM_THREADS=${BRIDGE_MSUB_NCORE}

set -x 


bash -c "cd .. && ./make_xios --arch ${arch} --${mode} --build_dir ${build_dir} --job 4"

#----------- Fetch reference -----------
cp ${reference_dir}/${reference_file} ./
tar -xzvf ${reference_file}

rm -f ${reference_file}

for i in $(ls ${reference_folder}/) 
do
  folder=${i%%}
  bash -c "cd ${folder} && rm -rf config_*"
  bash -c "cp -r ${reference_folder}/${folder}/config_* ./${folder}"
done

rm -rf ${reference_folder}


rm -f plein_report.txt

echo "#revision" ${xios_revision} >> plein_report.txt
echo "#relurl" ${relurl} >> plein_report.txt
echo "#machine" ${machine_name} >> plein_report.txt
echo "#build_dir" ${xios_dir}/${build_dir} >> plein_report.txt
echo "#arch" ${arch} >> plein_report.txt
echo "#mode" ${mode} >> plein_report.txt


rm -f setup.sh
touch setup.sh
>setup.sh
for i in $(ls -d test_*/)
do
  cp setup.py ${i%%/}      
  cp run_sub_test.sh ${i%%/}    
  echo "bash -c \"cd " ${i%%/} " && python setup.py\" ">> setup.sh
  echo "echo \"setup.py called from " ${i%%/} "\"">> setup.sh
done

export output=$(python user_config.py 2>&1 >/dev/null)

if [ "$output" -ne 0 ]
then
	echo "user_config.py failed"
	exit
else
	echo "user_config.py OK"
fi


cmake .
ctest -V 
#ctest --output-on-failure
make report

rm -f test_*/setup.py
rm -f test_*/run_sub_test.sh
rm -f test_*/run_test_*.py
rm -f test_*/CMakeLists.txt
rm -f test_*/context_grid_dynamico.xml
rm -f test_*/dynamico_grid.nc
rm -f test_*/default_param.pyc
rm -f test_*/user_param.pyc
rm -f test_*/user_param.py.*

mkdir -p ../html
mkdir -p ../html/test_irene

cp plein_report.txt ../html/test_irene/test_${xios_revision}_${machine_name}_${arch}_${mode}.txt

bash -c "cd ../html && python generate_test.py"





