import os
import sys

from default_param import *
from user_param import *
from param_list import *

def main():
	
	f=open("param.def", "w")

	f.write("&params_run\n")
	f.write('nb_proc_atm = ' + repr(NumberClients) + '\n')
	f.write('duration = ' + repr(Duration) + '\n')
	f.write('/\n\n')

	f.close()


	f=open("all_param.def", "w")

	f.write("&params_run\n")
	for i in range(len(param_list)):
		f.write(param_list[i]+" = "+repr(eval(param_list[i]))+ "\n")
	f.write('/\n\n')

	f.close()


	nb_proc = NumberClients+NumberServers

	filein = open("../iodef.xml.template", "r")
	fileout = open("iodef.xml", "w") #this iodef.xml has defined variables, it is not the template
	for line in filein:
		if 'xios::nbplSrv2' in line:	line = line.replace("xios::nbplSrv2", repr(NumberPoolsServer2))
		if 'xios::usingSrv2' in line:	line = line.replace("xios::usingSrv2", UsingServer2)
		if 'xios::ratioSrv2' in line:	line=line.replace("xios::ratioSrv2", repr(RatioServer2))
		if 'atm::domain' in line:	line=line.replace("atm::domain", ATMdomain)
		fileout.write(line)
		

	filein.close()
	fileout.close()


	print >>sys.stderr, nb_proc



if __name__== "__main__":
  main()
