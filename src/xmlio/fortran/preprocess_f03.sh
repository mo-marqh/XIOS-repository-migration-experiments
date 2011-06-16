#!/bin/bash
if [[ $# != 2 ]] ; then
   echo "Usage : ./prepocess_f03.sh <output> <input>"
   exit
fi

if test -f $2; then
   cpp -P -o $1.temp  $2;
   sed 's/;/\r\n/g' <$1.temp >$1
   rm -f $1.temp
else
   echo "le fichier d'entrée n'existe pas !"
fi


