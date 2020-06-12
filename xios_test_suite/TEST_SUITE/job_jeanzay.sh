#!/bin/bash
export build_dir=xios_test_suite/RUN_TEST_SUITE/build_${arch}_${mode}
export svnR=$(svn info --show-item revision ../../)
export ref_location=/gpfswork/rech/psl/rpsl954/cron_xios
export ref_file=reference.tar.gz

echo "Start Building XIOS ... "
bash -c "cd ../.. && ./make_xios --arch_path `pwd`/../ARCH --arch ${arch} --${mode} --build_dir ${build_dir} --job 4"

export build_status=$?
if [[ ${build_status} == 0 ]]
then
  echo "XIOS Build Finished. Start Unit Tests"
  bash ./my_prod.sh
  rundir=${xios_test_suite_repository}/RUN
  mkdir -p $rundir ; CHMOD  $rundir
  mkdir -p ${rundir}/test_${xios_machine_name} ; CHMOD ${rundir}/test_${xios_machine_name}

  cp report_${svnR}_${arch}_${mode}.txt ${rundir}/test_${xios_machine_name}/test_${svnR}_${xios_machine_name}_${arch}_${mode}.txt

  CHMOD ${rundir}/test_${xios_machine_name}/test_${svnR}_${xios_machine_name}_${arch}_${mode}.txt

  mkdir -p ${rundir}/def_files ;  CHMOD ${rundir}/def_files
  mkdir -p ${rundir}/def_files/${svnR} ;  CHMOD ${rundir}/def_files/${svnR}

  for i in $(ls -d test_*/)
  do
    mkdir -p ${rundir}/def_files/${svnR}/${i%%}
    cp ${i%%}/user_param.json ${rundir}/def_files/${svnR}/${i%%}
    for j in $(ls -d ${i%%/}/CONFIG_*)
    do
      mkdir -p ${rundir}/def_files/${svnR}/${j%%}
      cp ${j%%}/all_param.def ${rundir}/def_files/${svnR}/${j%%}
    done
    CHMOD ${rundir}/def_files
  done


else
  echo "XIOS Build Failed. Skip Unit Tests"
fi


