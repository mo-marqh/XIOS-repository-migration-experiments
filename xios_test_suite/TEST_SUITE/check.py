import os
import sys
import subprocess
import socket
import os.path
from os import path


# from default_param import *
# from user_param import *     
from param_list import *

# load default_param first, variables defines in user_param will overwrite the default value 



def OSinfo(runthis):
	red = lambda text: '\033[0;31m' + text + '\033[0m'
	osstdout = subprocess.Popen(runthis, shell=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True)
	theInfo = osstdout.communicate()[0].strip()
	if osstdout.returncode!=0:
		print(red(runthis+" FAILED"))
		print >> sys.stderr, osstdout.returncode
		sys.exit()
	# else:
		# print(runthis+" OK")



blue = lambda text: '\033[0;34m' + text + '\033[0m'
yellow = lambda text: '\033[0;33m' + text + '\033[0m'
green = lambda text: '\033[0;32m' + text + '\033[0m'
# blue = lambda text: text
# yellow = lambda text: text
# green = lambda text: text

def main():

	defaultConfig = True
	configNamePattern=[]
	for i in range(len(param_list)):
		f=open("user_param.py", "r")
		for line in f:
			line = line.strip().replace(" ","")
			if line.startswith(param_list[i]):
				configNamePattern.append('_'+param_short_list[i]+"="+line.split("=")[1].replace("'",""))
				defaultConfig = False
	
	f.close()
    
	configName = "config"
	
	for i in range(len(configNamePattern)):
		configName = configName+configNamePattern[i]

	checkfile = "checkfile.def"

	f=open(checkfile, "r")
	for line in f:
		line = line.strip()
		line = line.rstrip()
		if line.startswith("all"):
			allfiles = os.listdir(os.getcwd())
			g=open("checkfile_new.def", "w")
			for thisfile in allfiles:
				if thisfile.endswith(".nc") and thisfile != "dynamico_grid.nc":
					g.write(thisfile+"\n")
					checkfile = "checkfile_new.def"
			g.close()
			break
	f.close()

	if not os.path.exists(configName):
		OSinfo("mkdir -p "+configName)

		print(blue("\n    ****************************************************************"))
		print(blue("    * Directory " + repr(configName) + " Created "))
		print(blue("    * Copy results to \'not_validated_reference\' folder"))
		print(blue("    ****************************************************************\n"))

		OSinfo("mkdir -p "+configName+"/setup")
		OSinfo("mkdir -p "+configName+"/tmp_reference")

		OSinfo("cp user_param.py user_param.def")
		OSinfo("cp user_param.py "+configName+"/setup")
		OSinfo("mv user_param.def "+configName+"/setup")

		OSinfo("cp default_param.json "+configName+"/setup")
		
		OSinfo("mv all_param.def "+configName+"/setup")
		OSinfo("mv iodef.xml "+configName+"/setup/")
		OSinfo("cp context_atm.xml "+configName+"/setup/")
		OSinfo("cp context_grid_dynamico.xml "+configName+"/setup/")


		f=open(checkfile, "r")
		reportfile = open("../plain_report.txt", "a+")
		for line in f:
			line = line.strip()
			line = line.rstrip()
			if not line.startswith("#") and len(line) != 0:
				OSinfo("cp "+line+" "+configName+"/tmp_reference")
				reportfile.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(0)+" \n")
				print(yellow("\n        ****************************************"))
				print(yellow("        ** "+line+" is stored as temporal reference !!! **"))
				print(yellow("\n        ****************************************"))
		f.close()
		reportfile.close()

	else:
		print(blue("\n    ****************************************************************"))
		print(blue("    * Directory " + repr(configName) + " already exists "))

		tmp_ref = os.path.isdir(configName+'/tmp_reference')
		ref = os.path.isdir(configName+'/reference') and not tmp_ref
		
		if tmp_ref : 
			print(blue("    * Start comparing results with NON-validated references"))
			path = configName+"/tmp_reference"
		if ref : 
			print(blue("    * Start comparing results with validated references"))
			path = configName+"reference"
		
		OSinfo('cp '+configName+'/setup/user_param.py '+configName+'/setup/user_param.def ')
		print(blue("    ****************************************************************\n"))

		f=open(checkfile, "r")
		reportfile = open("../plain_report.txt", "a+")
		for line in f:
			line = line.strip()
			line = line.rstrip()
			if not line.startswith("#") and len(line) != 0:
				OSinfo("cdo -W diffn "+line+" "+path+line+" | > diff.txt")
				# this command should change according to cdo version

				if os.stat("diff.txt").st_size==0:
					reportfile.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(1)+" \n")
					print(green("\n        ****************************************"))
					print(green("        ** "+line+" is valid !!! **"))
					print(green("\n        ****************************************"))
				else:
					g=open("diff.txt", "r")
					for gline in g:
						if gline.strip().startswith("0") or ":" in gline:
							reportfile.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(1)+" \n")
							print(green("\n        ****************************************"))
							print(green("        ** "+line+" is valid !!! **"))
							print(green("\n        ****************************************"))
						else:
							reportfile.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(-1)+" \n")
							print(green("\n        ****************************************"))
							print(green("        ** "+line+" is NOT valid. Please debugging.. **"))
							print(green("\n        ****************************************"))
					g.close()
		f.close()
		reportfile.close()

	if os.path.exists("checkfile_new.def"):
		OSinfo("rm -f checkfile_new.def")



if __name__== "__main__":
  main()
