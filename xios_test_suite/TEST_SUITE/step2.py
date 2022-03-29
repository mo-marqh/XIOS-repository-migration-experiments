import glob
import sys
import subprocess
import os
import json
import itertools
import copy

import netCDF4
from netCDF4 import Dataset
import numpy as np

mode=os.getenv("mode")
arch=os.getenv("arch")
enable_mem_track=os.getenv("enable_mem_track")
svnr=os.getenv("svnR")
ref_location=os.getenv("ref_location")
ref_file=os.getenv("ref_file")



def OSinfo(runthis):
    red = lambda text: '\033[0;31m' + text + '\033[0m'
    osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True)
    theInfo, theErr = osstdout.communicate()
    #print(theInfo)
    if theErr:
        print(red(runthis+" FAILED"))
        print(theErr)
        sys.exit()

#def OSinfo(runthis):
#    red = lambda text: '\033[0;31m' + text + '\033[0m'
#    osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
#    theInfo = osstdout.communicate()[0].strip()
#    if osstdout.returncode!=0:
#        print(red(runthis+" FAILED"))
#        print(theInfo)
#        sys.exit()



def nonblank_lines(f):
    for l in f:
        line = l.rstrip()
        if line and not line.startswith("#"):
            yield line


def extract_bytes( filename ):
    print( filename )
    fmem = open(filename, 'r')
    bytes_tot = 0
    for line in fmem:
        if not line.startswith("***"):
            continue
        
        bytes_line = line.replace("\n", "")
        print( bytes_line.split() )
        bytes_str = bytes_line.split()[2]
        if (bytes_str.isdigit() ):
            bytes_tot += int( bytes_str )
    fmem.close()
  
    return bytes_tot


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
        report_filename=""
        # check if test concerns xios features (NetCDF), or memory consumption (mem files)
        files_list=""
        flist = open(test_folder+"/checkfile.def", 'r')
        files_list = flist.read()
        flist.close()
        if ( enable_mem_track==None ) and ( not('.mem' in files_list) ) :
            report_filename = "report_"+svnr+"_"+arch+"_"+mode+".txt"
        elif ( enable_mem_track=='--memtrack full' ) and ( '.mem' in files_list ) :
            #report_filename = "report_"+svnr+"_"+arch+"_"+mode+"_memtrack.txt"
            # -> use same report that for feature test
            report_filename = "report_"+svnr+"_"+arch+"_"+mode+".txt"
        else :
            continue

        config_list = glob.glob(test_folder+"/CONFIG_*")
        
        
        with open(test_folder+"/checkfile.def", "r") as fh:
            checkfiles = list(nonblank_lines(fh))

        with open(report_filename, "a") as report:
            for config in config_list:
                folder_name = list(config.split("/"))[0]
                config_name = list(config.split("/"))[1]
                for checkfile in checkfiles:
                    if os.path.exists(config+"/"+checkfile) and os.path.exists("reference/ref_"+config+"/"+checkfile):
                        if ( enable_mem_track==None ) and ( not('.mem' in files_list) ): # NetCDF
                            #OSinfo("cdo -W diffn "+config+"/"+checkfile+" "+"reference/ref_"+config+"/"+checkfile+"  2>&1 |grep -v 'Found more than one time variable'|grep -v 'cdo diffn: Processed'|grep -v 'cdo    diffn: Processed'|grep -v 'Time variable >time_counter< not found!' > diff_"+checkfile+".txt")
                            #if os.stat("diff_"+checkfile+".txt").st_size==0: # if no diff -> set 0
                            #    report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(1)+"\n")
                            #else: # if cdo diffn returns diff -> set -1
                            #    report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(-1)+"\n")
                            ref = Dataset( "reference/ref_"+config+"/"+checkfile )
                            res = Dataset( config+"/"+checkfile )
                            validated = 1
                            np.seterr(divide='ignore', invalid='ignore')
                            for var in res.variables:
                                print("checkfile = ", checkfile, ", var = ", var)
                                if (not (var.startswith('lon_'))) and (not (var.startswith('lat_'))) and (not (var.startswith('time_'))) and (not (var.startswith('atm__')) and (var!="lat") and (var!="lon") ):
                                    ref_interp = ref.variables[var]
                                    ref_array = ref_interp[:]
                                    res_interp = res.variables[var]
                                    res_array = res_interp[:]
                                    if (res_array.shape == ref_array.shape):
                                        diff = np.zeros_like( ref_array )
                                        np.divide(ref_array-res_array,ref_array,diff,where=(ref_array[:]>10**-15))
                                        if ( np.max(np.abs(diff)) >  1*10**-9 ):
                                            validated = -1
                                        diff = np.zeros_like( ref_array )
                                        np.divide(ref_array-res_array,res_array,diff,where=(ref_array[:]>10**-15))
                                        if ( np.max(np.abs(diff)) >  1*10**-9 ):
                                            validated = -1
                                    else:
                                        validated = -1
                            report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(validated)+"\n")
                        
                        elif ( enable_mem_track=='--memtrack full' ) and ( '.mem' in files_list ) : # mem file
                            validated = 1
                            ref_memory = extract_bytes( "reference/ref_"+config+"/"+checkfile )
                            res_memory = extract_bytes( config+"/"+checkfile )
                            if ( ref_memory == res_memory ) :
                                validated = 1
                            else:
                                validated = -1
                            report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(validated)+"\n")

                    elif os.path.exists(config+"/"+checkfile): # if no ref file -> set 0
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(0)+"\n")
                    elif os.path.exists("reference/ref_"+config+"/"+checkfile): # if no output file -> set -2
                        report.write(folder_name+" "+folder_name+"@"+config_name+" "+folder_name+"@"+config_name+"@"+checkfile+" "+str(-2)+"\n")

                   

if __name__== "__main__":
  main()
