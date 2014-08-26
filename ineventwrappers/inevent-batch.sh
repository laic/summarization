#!/bin/bash

PREFIX=$1
for file in `ls ~/data/inevent/UEDIN_ASR_201407/$PREFIX*`
do 
	fstem=`basename $file`
	echo  $fstem
	./inevent-proc.sh $fstem
done
