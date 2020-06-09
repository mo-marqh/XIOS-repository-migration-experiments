import glob
import sys
import subprocess
import os
import json
import itertools
import copy

mode=os.getenv("mode")
arch=os.getenv("arch")
svnr=os.getenv("svnR")
repository=os.getenv("xios_test_suite_repository")

def OSinfo(runthis):
    red = lambda text: '\033[0;31m' + text + '\033[0m'
    osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
    theInfo = osstdout.communicate()[0].strip()
    if osstdout.returncode!=0:
        print(red(runthis+" FAILED"))
        print >> sys.stderr, osstdout.returncode
        sys.exit()
    # else:
    #     print(runthis+" OK")


def get_default_param():
    f=open("default_param.json", 'r')
    default_param = json.load(f)
    f.close()
    return default_param[0]

def nonblank_lines(f):
    for l in f:
        line = l.rstrip()
        if line and not line.startswith("#"):
            yield line

def main():
    print(svnr) 
    print(repository)
    test_folder_list = glob.glob('test_*')

    for test_folder in test_folder_list:
        OSinfo("cp "+test_folder+"/user_param.json "+repository+"/RUN/def_files/"+svnr+"/"+test_folder)
        config_list = glob.glob(test_folder+"/CONFIG_*")
        #print(*config_list, sep = "\n")
        
        
                   

if __name__== "__main__":
  main()
