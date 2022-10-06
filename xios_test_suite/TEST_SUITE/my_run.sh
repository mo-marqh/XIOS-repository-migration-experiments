#!/bin/bash 

echo "running my_run.sh"

export svnR=$(svn info --show-item revision ../../)
svnurl=$(svn info --show-item url ../../)
PWD=$(pwd)

fn=report_${svnR}_${arch}_${mode}.txt

if [ -z ${enable_mem_track+x} ]; then
  # initialize the report for the main part of the test (without memory tracking, see step2.py for details)
  echo "#revision" ${svnR} > ${fn}
  echo "#url" ${svnurl} >> ${fn}
  echo "#machine" ${xios_machine_name} >> ${fn}
  echo "#build_dir" $(pwd)/build_${arch}_${mode} >> ${fn}
  echo "#arch" $arch >> ${fn}
  echo "#mode" $mode >> ${fn}
else
  if  [ ! -f "$fn" ]; then 
    # initialize the report if it does not exist
    echo "#revision" ${svnR} > ${fn}
    echo "#url" ${svnurl} >> ${fn}
    echo "#machine" ${xios_machine_name} >> ${fn}
    echo "#build_dir" $(pwd)/build_${arch}_${mode} >> ${fn}
    echo "#arch" $arch >> ${fn}
    echo "#mode" $mode >> ${fn}
    echo "#memtrack full" >> ${fn}
  fi
  # else write in the same report
fi

${PYTHON} step1.py


if [[ ${xios_machine_name} == "irene" ]]; then
  cmd=$(ccc_msub full_job_${arch}_${mode}.sh)
  jobid="${cmd//[!0-9]/}"

  i=0
  output=$(squeue -u $USER | grep ${jobid})
  while [ ! -z "$output" ]
  do
    echo "job" $jobid "pending/running for about" ${i} seconds
    sleep 30
    ((i+=30))
    output=$(squeue -u $USER | grep ${jobid})
    if [[ $i -eq 300 ]]; then
      ccc_mdel $jobid
      break
    fi
  done
fi



if [[ ${xios_machine_name} == "jeanzay" ]]; then
  cmd=$(sbatch full_job_${arch}_${mode}.sh)
  jobid="${cmd//[!0-9]/}"
  i=0
  output=$(squeue -u uim55ri | grep ${jobid})
  while [ ! -z "$output" ]
  do
    echo "job" $jobid "pending/running for about" ${i} seconds
    sleep 30
    ((i+=30))
    if [[ $i -eq 300 ]]; then
      output=$(squeue -u uim55ri | grep ${jobid})
      scancel $jobid
      break
    fi
  done
fi


${PYTHON} step2.py
