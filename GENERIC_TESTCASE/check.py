import os
import sys
import subprocess
import socket
import os.path
from os import path


from default_param import *
from user_param import *     

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


def write_jz_job(configName, nbproc=8):
	red = lambda text: '\033[0;31m' + text + '\033[0m'
	f=open("jz_job_"+configName+".sh", "w")
	f.write("#!/bin/bash\n")
	f.write("#SBATCH --ntasks="+repr(nbproc)+"              # Nombre total de processus MPI\n")
	f.write("#SBATCH --ntasks-per-node=16\n")
	f.write("#SBATCH --hint=nomultithread\n")
	f.write("#SBATCH -t 00:10:00            # Temps elapsed maximum d'execution\n")
	f.write("#SBATCH -o single_config.out     # Nom du fichier de sortie\n")
	f.write("#SBATCH -e single_config.err     # Nom du fichier d'erreur\n")
	f.write("#SBATCH --account=psl@cpu\n")
	f.write("cd ${SLURM_SUBMIT_DIR}\n")
	f.write("module purge\n")
	f.write("module load intel-all/19.0.4\n")
	f.write("module load netcdf/4.7.0/intel-19.0.4-mpi\n")
	f.write("module load netcdf-cxx/4.2/intel-19.0.4-mpi\n")
	f.write("module load netcdf-fortran/4.4.5/intel-19.0.4-mpi\n")
	f.write("module load gcc/6.5.0/gcc-4.8.5\n")
	f.write("module load cmake\n")
	f.write("module load cdo\n")
	f.write("ulimit -c 0\n")
	xios_root=os.getenv('xios_dir')
	build_folder=os.getenv('build_dir')
	f.write("export xios_dir="+xios_root+"\n")
	f.write("export build_dir="+build_folder+"\n")
	f.write("export machine_name=jeanzay\n")
	f.write("cp ../setup.py ./\n")
	f.write("cp ../run_sub_test.sh ./\n")
	f.write("mv user_params.def user_params.def_bkp\n")
	f.write("cp "+configName+"/setup/user_param.py user_params.def\n")
	f.write("python setup.py\n")
	f.write("cmake .\n")
	f.write("ctest -V #enable all output\n")
	f.write("rm -f setup.py\n")
	f.write("rm -f run_sub_test.sh\n")
	f.write("rm -f run_test_*.py\n")
	f.write("rm -f CMakeLists.txt\n")
	f.write("rm -f context_grid_dynamico.xml\n")
	f.write("rm -f dynamico_grid.nc\n")
	f.write("rm -f default_param.*\n")
	f.write("rm -f user_param.*\n")
	f.write("mv user_params.def_bkp user_params.def\n")
	f.write("\n")
	f.close

def write_irene_job(configName, nbproc=8):
	red = lambda text: '\033[0;31m' + text + '\033[0m'
	f=open("irene_job_"+configName+".sh", "w")
	f.write("#!/bin/bash\n")
	f.write("#MSUB -r XIOS\n")
	f.write("#MSUB -o client_output.out    # standard output\n")
	f.write("#MSUB -e client_error.err    #error output\n")
	f.write("#MSUB -eo\n")
	f.write("#MSUB -c 1\n")
	f.write("#MSUB -n "+ repr(nbproc) + "  # Number of MPI tasks (SPMD case) or cores (MPMD case)\n")
	f.write("#MSUB -X \n")
	f.write("#MSUB -x \n")
	f.write("#MSUB -T 600      # Wall clock limit (seconds)\n")
	f.write("#MSUB -q skylake           # thin nodes\n")
	f.write("#MSUB -A devcmip6\n")
	f.write("#MSUB -Q test\n")
	f.write("#MSUB -m work\n")
	f.write("cd $BRIDGE_MSUB_PWD\n")
	f.write("module unload netcdf-c netcdf-fortran hdf5 flavor perl hdf5 boost blitz mpi gnu\n")
	f.write("module load gnu\n")
	f.write("module load mpi/intelmpi/2017.0.6.256\n")
	f.write("module load flavor/buildcompiler/intel/17\n")
	f.write("module load flavor/hdf5/parallel\n")
	f.write("module load netcdf-fortran/4.4.4\n")
	f.write("module load hdf5/1.8.20\n")
	f.write("module load boost\n")
	f.write("module load blitz\n")
	f.write("module load feature/bridge/heterogenous_mpmd\n")
	f.write("module load nco\n")
	f.write("module load cdo\n")
	f.write("export KMP_STACKSIZE=3g\n")
	f.write("export KMP_LIBRARY=turnaround\n")
	f.write("export MKL_SERIAL=YES\n")
	f.write("export OMP_NUM_THREADS=${BRIDGE_MSUB_NCORE}\n")
	f.write("set -x \n")
	xios_root=os.getenv('xios_dir')
	build_folder=os.getenv('build_dir')
	f.write("export xios_dir="+xios_root+"\n")
	f.write("export build_dir="+build_folder+"\n")
	f.write("export machine_name=irene\n")
	f.write("cp ../setup.py ./\n")
	f.write("cp ../run_sub_test.sh ./\n")
	f.write("mv user_params.def user_params.def_bkp\n")
	f.write("cp "+configName+"/setup/user_param.py user_params.def\n")
	f.write("python setup.py\n")
	f.write("cmake .\n")
	f.write("ctest -V #enable all output\n")
	f.write("rm -f setup.py\n")
	f.write("rm -f run_sub_test.sh\n")
	f.write("rm -f run_test_*.py\n")
	f.write("rm -f CMakeLists.txt\n")
	f.write("rm -f context_grid_dynamico.xml\n")
	f.write("rm -f dynamico_grid.nc\n")
	f.write("rm -f default_param.*\n")
	f.write("rm -f user_param.*\n")
	f.write("mv user_params.def_bkp user_params.def\n")
	f.write("\n")
	f.close



def main():

	black = lambda text: '\033[0;30m' + text + '\033[0m'
	red = lambda text: '\033[0;31m' + text + '\033[0m'
	green = lambda text: '\033[0;32m' + text + '\033[0m'
	yellow = lambda text: '\033[0;33m' + text + '\033[0m'
	blue = lambda text: '\033[0;34m' + text + '\033[0m'
	magenta = lambda text: '\033[0;35m' + text + '\033[0m'
	cyan = lambda text: '\033[0;36m' + text + '\033[0m'
	lgray = lambda text: '\033[0;37m' + text + '\033[0m'
	dgray = lambda text: '\033[1;30m' + text + '\033[0m'
	lred = lambda text: '\033[1;32m' + text + '\033[0m'
	lgreen = lambda text: '\033[1;32m' + text + '\033[0m'
	lyellow = lambda text: '\033[1;33m' + text + '\033[0m'
	lblue = lambda text: '\033[1;34m' + text + '\033[0m'
	lpurple = lambda text: '\033[1;35m' + text + '\033[0m'
	lcyan = lambda text: '\033[1;36m' + text + '\033[0m'
	white = lambda text: '\033[1;37m' + text + '\033[0m'
	
	configNamePattern=[0]*7

	f=open("user_param.py", "r")
	for line in f:
		line = line.strip()
		if line.startswith("UsingServer2"):
			configNamePattern[0] = '_UsingSrv2=' + UsingServer2
		elif line.startswith("RatioServer2"):
			configNamePattern[1] = '_RatioSrv2=' + repr(RatioServer2)
		elif line.startswith("NumberPoolsServer2"):
			configNamePattern[2] = '_NbPoolsSrv2=' + repr(NumberPoolsServer2)
		elif line.startswith("NumberClients"):
			configNamePattern[3] = '_NbClients=' + repr(NumberClients)
		elif line.startswith("NumberServers"):
			configNamePattern[4] = '_NbServers=' + repr(NumberServers)
		elif line.startswith("Duration"):
			configNamePattern[5] = '_Duration=' + Duration
		elif line.startswith("ATMdomain"):
			configNamePattern[5] = '_ATMdomain=' + ATMdomain
	f.close()
		
	

	configName = "config"
	defaultConfig = True

	for i in range(len(configNamePattern)):
		if configNamePattern[i] != 0:
			configName = configName + configNamePattern[i]
			defaultConfig = False

	if defaultConfig:
		configName = "config_default"

	


	if not os.path.exists(configName):

		
		

		os.mkdir(configName)
		print blue("\n    ****************************************************************")
		print blue("    * Directory " + repr(configName) + " Created ")
		print blue("    * Copy results to \'not_validated_reference\' folder")
		print blue("    ****************************************************************\n")

		OSinfo("mkdir "+configName+'/setup')
		OSinfo('mkdir '+configName+'/tmp_reference')
		OSinfo('mkdir '+configName+'/xios_output')
		
		OSinfo('mv param.def '+configName+'/setup/')
		OSinfo('mv all_param.def '+configName+'/setup/')
		OSinfo('mv user_param.py '+configName+'/setup/')
		OSinfo('cp '+configName+'/setup/user_param.py '+configName+'/setup/user_param.def ')
		OSinfo('mv iodef.xml '+configName+'/setup/')
		OSinfo('cp context_atm.xml '+configName+'/setup/')
		OSinfo('cp context_grid_dynamico.xml '+configName+'/setup/')
		OSinfo('cp dynamico_grid.nc '+configName+'/setup/')
		OSinfo('mv xios_*.out '+configName+'/xios_output/')
		OSinfo('mv xios_*.err '+configName+'/xios_output/')
		

		f=open("checkfile.def", "r")
		h=open("report.txt", "a+")
		l=open("report.html", "a+")
		preport=open("../plein_report.txt", "a+")
		h.write("Config : "+configName+"\n")		
		l.write("<p>Config : "+configName+"</p>")	
		for line in f:
			line=line.strip()
			line=line.rstrip()

			if line.startswith("all"):
				allfiles = os.listdir(os.getcwd())
				for thisfile in allfiles:
					if thisfile.endswith(".nc") and thisfile!="dynamico_grid.nc":
						cmd = 'mv '+thisfile + ' '+configName+'/tmp_reference/'
						OSinfo(cmd)	
				
				h.write("test for "+lpurple("all NC files")+"     \t"+yellow('INITIALIZED')+"\n")
				preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"_all_nc_files"+" "+repr(0)+" \n")				
				l.write("<p>test for <font color=\"purple\">"+"all NC files"+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"darkorange\">INITIALIZED</font></p>")
				print yellow("\n        ****************************************")
				print yellow("        ** "+"all NC files"+" is stored as temporal reference !!! **")
				print yellow("\n        ****************************************")

				break

			if not line.startswith("#") and len(line) != 0:
				cmd = 'mv '+line + ' '+configName+'/tmp_reference/'
				OSinfo(cmd)

				h.write("test for "+lpurple(line)+"     \t"+yellow('INITIALIZED')+"\n")		
				preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(0)+" \n")		
				l.write("<p>test for <font color=\"purple\">"+line+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"darkorange\">INITIALIZED</font></p>")
				print yellow("\n        ****************************************")
				print yellow("        ** "+line+" is stored as temporal reference !!! **")
				print yellow("\n        ****************************************")
				
		f.close()
		h.write("\n")
		l.write("<br>")
		h.close()
		l.close()
		preport.close()
		#generate job scripts
		hostname=os.getenv('machine_name')
		if hostname.startswith("jeanzay"):
			write_jz_job(configName, NumberClients+NumberServers)
				
		elif hostname.startswith("irene"):
			write_irene_job(configName, NumberClients+NumberServers)
							
		else :
			print("machine unknown")
			print >> sys.stderr, 1
			sys.exit()


	else:    
		


		print blue("\n    ********************************************************")
		print blue("    * Directory " + repr(configName) + " already exists")
		print blue("    * Start comparing results with references")
		print blue("    ********************************************************\n")

		tmp_ref = os.path.isdir(configName+'/tmp_reference')
		ref = os.path.isdir(configName+'/reference') and not tmp_ref


		if ref:
			print blue("\n    =====================================")
			print blue("    | Comparing results with references |")
			print blue("    =====================================\n")
			path = configName+'/reference/'
			OSinfo('cp '+configName+'/setup/user_param.py '+configName+'/setup/user_param.def ')
		elif tmp_ref:
			print blue("\n    ===================================================")
			print blue("    | Comparing results with NON-Validated references |")
			print blue("    ===================================================\n")
			path = configName+'/tmp_reference/'
			OSinfo('cp '+configName+'/setup/user_param.py '+configName+'/setup/user_param.def ')
		
		f=open("checkfile.def", "r")
		h=open("report.txt", "a+")
		l=open("report.html", "a+")
		preport=open("../plein_report.txt", "a+")


		h.write("Config : "+configName+"\n")	
		l.write("<p>Config : "+configName+"</p>")	
		for line in f:

			line=line.strip()
			line=line.rstrip()
			
			if line.startswith("all") :
				for thisfile in os.listdir(path):
					if thisfile.endswith(".nc"):
						cmd = 'cdo -W diffn '+ thisfile +' '+ path + thisfile + ' | tail -1 > diff.txt'
						
						OSinfo(cmd)
						
						if os.stat("diff.txt").st_size==0:
							h.write("test for "+lpurple(thisfile)+"     \t"+green('PASSED')+"\n")		
							preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+thisfile+" "+repr(1)+" \n")		
							l.write("<p>test for <font color=\"purple\">"+thisfile+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"green\">PASSED</font></p>")		
							print green("\n        ****************************************")
							print green("        ** "+thisfile+" is valid !!! **")
							print green("\n        ****************************************")

						else:	
							g=open("diff.txt", "r")
							for gline in g:
								if gline.strip().startswith("0") or ":" in gline :
									h.write("test for "+lpurple(thisfile)+"      \t"+green('PASSED')+"\n")	
									preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+thisfile+" "+repr(1)+" \n")			
									l.write("<p>test for <font color=\"purple\">"+thisfile+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"green\">PASSED</font></p>")		
									print green("\n        ****************************************")
									print green("        ** "+thisfile+" is valid !!! **")
									print green("\n        ****************************************")
								else:
									h.write("test for "+lpurple(thisfile)+"     \t"+red('FAILED')+"\n")	
									preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+thisfile+" "+repr(-1)+" \n")			
									l.write("<p>test for <font color=\"purple\">"+thisfile+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"red\">FAILED</font></p>")		
									print red("\n        **************************************************")
									print red("        ** "+thisfile+" is NOT valid. Please debugging.. **")
									print red("\n        **************************************************")
							g.close()

						# generate job submission script
						hostname=os.getenv('machine_name')

						if hostname.startswith("jeanzay"):
							write_jz_job(configName, NumberClients+NumberServers)
							
						elif hostname.startswith("irene"):
							write_irene_job(configName, NumberClients+NumberServers)
							
						else :
							print("machine unknown")
							print >> sys.stderr, 1
							sys.exit()
				break


			elif not line.startswith("#") and len(line) != 0:
				cmd = 'cdo -W diffn '+ line +' '+ path + line + ' | tail -1 > diff.txt'
				
				OSinfo(cmd)

				if os.stat("diff.txt").st_size==0:
					h.write("test for "+lpurple(line)+"     \t"+green('PASSED')+"\n")
					preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(1)+" \n")				
					l.write("<p>test for <font color=\"purple\">"+line+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"green\">PASSED</font></p>")		
					print green("\n        ****************************************")
					print green("        ** "+line+" is valid !!! **")
					print green("\n        ****************************************")

				else:	
					g=open("diff.txt", "r")
					for gline in g:
						if gline.strip().startswith("0") or ":" in gline :
							h.write("test for "+lpurple(line)+"      \t"+green('PASSED')+"\n")		
							preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(1)+" \n")		
							l.write("<p>test for <font color=\"purple\">"+line+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"green\">PASSED</font></p>")		
							print green("\n        ****************************************")
							print green("        ** "+line+" is valid !!! **")
							print green("\n        ****************************************")
						else:
							h.write("test for "+lpurple(line)+"     \t"+red('FAILED')+"\n")	
							preport.write(os.path.basename(os.getcwd())+" "+os.path.basename(os.getcwd())[5:]+"@"+configName+" "+os.path.basename(os.getcwd())[5:]+"@"+configName[7:]+"@"+line+" "+repr(-1)+" \n")			
							l.write("<p>test for <font color=\"purple\">"+line+"</font> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <font color=\"red\">FAILED</font></p>")		
							print red("\n        **************************************************")
							print red("        ** "+line+" is NOT valid. Please debugging.. **")
							print red("\n        **************************************************")
					g.close()

				# generate job submission script
				hostname=os.getenv('machine_name')

				if hostname.startswith("jeanzay"):
					write_jz_job(configName, NumberClients+NumberServers)
							
				elif hostname.startswith("irene"):
					write_irene_job(configName, NumberClients+NumberServers)
							
				else :
					print("machine unknown")
					print >> sys.stderr, 1
					sys.exit()


		f.close()
		h.write("\n")
		l.write("<br>")
		h.close()
		l.close()
		preport.close()

		if not os.path.exists('current_run'):
			OSinfo('mkdir current_run')

		if not os.path.exists('current_run/'+configName):
			OSinfo('mkdir current_run/'+configName)

		OSinfo('mv -f param.def current_run/'+configName)
		OSinfo('mv -f all_param.def current_run/'+configName)
		OSinfo('mv -f user_param.py current_run/'+configName)
		OSinfo('mv -f iodef.xml current_run/'+configName)
		OSinfo('rm -f current_run/'+configName+'/context_atm.xml')
		OSinfo('cp -n context_atm.xml current_run/'+configName)
		OSinfo('cp -f context_grid_dynamico.xml current_run/'+configName)
		OSinfo('cp -f dynamico_grid.nc current_run/'+configName)
		OSinfo('mv -f xios_*.out current_run/'+configName)
		OSinfo('mv -f xios_*.err current_run/'+configName)




if __name__== "__main__":
  main()
