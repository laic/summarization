#!/bin/bash

PREFIX=$1
wtype=$2 	#asrutt
segsdir=$3

for file in `ls ~/data/inevent/UEDIN_ASR_201407/$PREFIX*`
do 
	conv=`basename $file .json`
	echo  $conv
	./inevent-pros-da-aggs.sh $conv $wtype $segsdir
done
