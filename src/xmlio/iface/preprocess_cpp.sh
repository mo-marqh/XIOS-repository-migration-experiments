#!/bin/bash
if [[ $# != 2 ]] ; then
   echo "Usage : ./prepocess_cpp.sh <output> <input>"
   exit
fi

if test -f $2; then
   cpp -x c++ -P -o $1.0.temp  $2;
   sed 's/;;/\r\n/g' <$1.0.temp >$1.1.temp
   sed 's/INCLUDE/#include/g' <$1.1.temp >$1.0.temp
   sed 's/\r\n \r\n/\r\n/g' <$1.0.temp >$1
   rm -f $1.0.temp $1.1.temp
else
   echo "le fichier d'entrée n'existe pas !"
fi


