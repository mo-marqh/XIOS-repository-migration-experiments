#!/bin/bash
revision=$(svn info --show-item revision ../ 2>&1)
mkdir -p xios_reference
for i in $(ls -d test_*)
do
  echo ${i%%}
  mkdir -p xios_reference/${i%%}
  for j in $(ls -d ${i%%}/config_*)
  do
    echo ${j%%}
    mkdir -p xios_reference/${j%%}
    DIR=${j%%}/tmp_reference
    if [ -d "$DIR" ]; then
      mv ${j%%}/tmp_reference ${j%%}/reference  
    fi

    cp -r ${j%%}/reference xios_reference/${j%%}
    cp -r ${j%%}/setup xios_reference/${j%%}
  done

done

cp -r ../ENV ./
cp ../load_env ./
source ./load_env
#echo "xios_reference_dir" ${xios_reference_dir}

mkdir -p ${xios_reference_dir}/xios_reference_archive
mkdir -p ${xios_reference_dir}/xios_reference_archive/$(( ${revision}-1 ))
mv ${xios_reference_dir}/xios_reference.tar.gz ${xios_reference_dir}/xios_reference_archive/$(( ${revision}-1 ))/

tar -zcvf ${xios_reference_dir}/xios_reference.tar.gz xios_reference/
rm -rf xios_reference

rm -rf ENV
rm -f load_env
