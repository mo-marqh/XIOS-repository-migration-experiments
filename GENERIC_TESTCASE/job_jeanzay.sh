#!/bin/bash
##SBATCH --nodes=2
#SBATCH --ntasks=8              # Nombre total de processus MPI
#SBATCH --ntasks-per-node=16
##SBATCH --cpus-per-task=1
#SBATCH --hint=nomultithread
#SBATCH -t 00:10:00              # Temps elapsed maximum d'exécution
#SBATCH -o output.out     # Nom du fichier de sortie
#SBATCH -e error.err     # Nom du fichier d'erreur
##SBATCH -p cpu_port             # Nom de la partition d'exécution
##SBATCH -A sos@cpu
##SBATCH --distribution=arbitrary
#SBATCH --account=psl@cpu


cd ${SLURM_SUBMIT_DIR}


#============================= X64_JEANZAY_prod =============================

export arch=X64_JEANZAY
export mode=prod
export machine_name=jeanzay
export machine_full_name=Jean-Zay
export xios_dir=/gpfswork/rech/psl/rpsl954/cron_xios/trunk
export reference_dir=${WORK}
export reference_file=xios_reference.tar.gz
export reference_folder=xios_reference

export build_dir=build_${arch}_${mode}
export xios_revision=$(svn info --show-item revision .. 2>&1)
export relurl=$(svn info --show-item relative-url .. 2>&1)

source ../arch/arch-${arch}.env
source $I_MPI_ROOT/intel64/bin/mpivars.sh release_mt

module load cmake
module load cdo
module load subversion

ulimit -c 0

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
mkdir -p ../html/test_${machine_name}
cp plein_report.txt ../html/test_${machine_name}/test_${xios_revision}_${machine_name}_${arch}_${mode}.txt


mkdir -p ../html/def_files
mkdir -p ../html/def_files/${xios_revision}

for i in $(ls -d test_*/)
do
  mkdir -p ../html/def_files/${xios_revision}/${i%%}
  cp ${i%%}/user_params.def ../html/def_files/${xios_revision}/${i%%}      
  for j in $(ls -d ${i%%/}/config_*)
  do
    mkdir -p ../html/def_files/${xios_revision}/${j%%}
    cp ${j%%}/setup/all_param.def ../html/def_files/${xios_revision}/${j%%}  
  done
done


bash -c "cd ../html && python generate_test.py"



#============================= X64_JEANZAY_debug =============================

export arch=X64_JEANZAY
export mode=debug
export machine_name=jeanzay
export machine_full_name=Jean-Zay
export xios_dir=/gpfswork/rech/psl/rpsl954/cron_xios/trunk
export reference_dir=${WORK}
export reference_file=xios_reference.tar.gz
export reference_folder=xios_reference

export build_dir=build_${arch}_${mode}
export xios_revision=$(svn info --show-item revision .. 2>&1)
export relurl=$(svn info --show-item relative-url .. 2>&1)

source ../arch/arch-${arch}.env
source $I_MPI_ROOT/intel64/bin/mpivars.sh release_mt

module load cmake
module load cdo
module load subversion

ulimit -c 0

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
mkdir -p ../html/test_${machine_name}

cp plein_report.txt ../html/test_${machine_name}/test_${xios_revision}_${machine_name}_${arch}_${mode}.txt

mkdir -p ../html/def_files
mkdir -p ../html/def_files/${xios_revision}

for i in $(ls -d test_*/)
do
  mkdir -p ../html/def_files/${xios_revision}/${i%%}
  cp ${i%%}/user_params.def ../html/def_files/${xios_revision}/${i%%}      
  for j in $(ls -d ${i%%/}/config_*)
  do
    mkdir -p ../html/def_files/${xios_revision}/${j%%}
    cp ${j%%}/setup/all_param.def ../html/def_files/${xios_revision}/${j%%}  
  done
done


bash -c "cd ../html && python generate_test.py"

