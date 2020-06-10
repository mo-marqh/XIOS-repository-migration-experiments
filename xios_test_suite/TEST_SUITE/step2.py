import glob
import sys
import subprocess
import os
import json
import itertools
import copy

param_list = ["ATMdomain", "UsingServer2", "NumberClients", "NumberServers", "RatioServer2", "NumberPoolsServer2", "Duration"]
param_short_list = ["ATMdom", "Srv2", "NbClnt", "NbSrv", "RatioSrv2", "NbPlSrv2", "Duration"]

mode=os.getenv("mode")
arch=os.getenv("arch")
svnr=os.getenv("svnR")
ref_location=os.getenv("ref_location")
ref_file=os.getenv("ref_file")

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
    OSinfo("cp "+ref_location+"/"+ref_file+" ./")
    OSinfo("tar -zxvf "+ref_file)
    OSinfo("rm -f "+ref_file)
    test_folder_list = glob.glob('test_*')

    for test_folder in test_folder_list:
        config_list = glob.glob(test_folder+"/CONFIG_*")
        #print(*config_list, sep = "\n")
        
        
        with open(test_folder+"/checkfile.def", "r") as fh:
            checkfiles = list(nonblank_lines(fh))

        with open("report_"+svnr+"_"+arch+"_"+mode+".txt", "a") as report:
            for config in config_list:
                folder_name = list(config.split("/"))[0]
                config_name = list(config.split("/"))[1]
                for checkfile in checkfiles:
                    if os.path.exists(config+"/"+checkfile) and os.path.exists("reference/ref_"+config+"/"+checkfile):
                        OSinfo("cdo -W diffn "+config+"/"+checkfile+" "+"reference/ref_"+config+"/"+checkfile+" | > diff.txt")
                        if os.stat("diff.txt").st_size==0:
                            report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(1)+"\n")
                    elif os.path.exists(config+"/"+checkfile):
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(0)+"\n")
                   

if __name__== "__main__":
  main()
