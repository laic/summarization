#!/bin/bash

PREFIX=$1
wtype=$2
CORPUS=inevent
for file in `ls ~/data/inevent/UEDIN_ASR_201407/$PREFIX*`
do 
	conv=`basename $file .json`
	echo  $conv
	./inevent-apply-mods-sub.sh "aug.wsw" "$conv.$CORPUS.group.fx0.aug.wsw.$wtype" "ami.group.fx0.aug.wsw" $wtype
done
