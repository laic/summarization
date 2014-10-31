#!/bin/bash

PREFIX=$1
for file in `ls ~/data/inevent/UEDIN_ASR_201407/$PREFIX*`
do 
	conv=`basename $file .json`
	echo  $conv
	./inevent-autopunc-conv.sh $conv
done
