#!/bin/bash

PREFIX=$1
wtype=$2

datadir="~/data/inevent/derived"
for sentfile in `ls ~/data/inevent/derived/segs/reval/ami.group.fx0.aug.wsw/$PREFIX*.tf.pros_pros.$wtype.eval.txt`
do 
	echo  $sentfile
	qsub ../sgescripts/get-ed-write-utt-prob.sh $sentfile $datadir $wtype

done
