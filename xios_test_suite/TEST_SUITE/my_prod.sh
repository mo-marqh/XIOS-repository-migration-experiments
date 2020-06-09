#!/bin/bash

echo "running my_prod.sh"

export svnR=$(svn info --show-item revision ../../)
svnurl=$(svn info --show-item url ../../)
PWD=$(pwd)

fn=report_${svnR}_${arch}_${mode}.txt
echo "#revision" ${svnR} > ${fn}
echo "#url" ${svnurl} >> ${fn}
echo "#machine" ${xios_machine_name} >> ${fn}
echo "#build_dir" $(pwd)/build_${arch}_${mode} >> ${fn}
echo "#arch" $arch >> ${fn}
echo "#mode" $mode >> ${fn}

python3 step1.py
cmd=$(ccc_msub full_job_${arch}_${mode}.sh)
jobid="${cmd//[!0-9]/}"
#jobid=9999999

i=0
output=$(ccc_mpp | grep ${jobid})
while [ ! -z "$output" ]
do
echo "job" $jobid "running for about" ${i} seconds
sleep 5
((i+=5))
output=$(ccc_mpp | grep ${jobid})
done

python3 step2.py
python3 step3.py
