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
ref_location=os.getenv("ref_location")
ref_file=os.getenv("ref_file")



def OSinfo(runthis):
    red = lambda text: '\033[0;31m' + text + '\033[0m'
    osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
    theInfo = osstdout.communicate()[0].strip()
    if osstdout.returncode!=0:
        print(red(runthis+" FAILED"))
        print(theInfo)
        sys.exit()



def nonblank_lines(f):
    for l in f:
        line = l.rstrip()
        if line and not line.startswith("#"):
            yield line

def main():
    ref_list = glob.glob(ref_location+"/*")
    for i in range(len(ref_list)):
        tmp = ref_list[i].split("/")
        rev = tmp[len(tmp)-1]
        ref_list[i] = int(rev)
    ref_list.sort(reverse=True) #ref_list in descending order
    
    ref_rev = ""
    for ref in ref_list:
        if int(svnr) >= ref :
            ref_rev = str(ref)
            print("corresponding reference = ", ref)
            break
        
    if not ref_rev:
        print("no available reference found ... exit")
        return
    
    OSinfo("cp "+ref_location+"/"+ref_rev+"/"+ref_file+" ./")
    OSinfo("tar -zxvf "+ref_location+"/"+ref_rev+"/"+ref_file)
    OSinfo("rm -f "+ref_file)
    
    
    test_folder_list = glob.glob('test_*')

    for test_folder in test_folder_list:
        config_list = glob.glob(test_folder+"/CONFIG_*")
        
        
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
