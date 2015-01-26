#!/bin/bash

PREFIX=$1
WTYPE=$2	## e.g. autosent0.7
WDIR=$3

SGESCRIPTS="../sgescripts/"
datadir="~/data/inevent/derived"

for sentfile in `ls ~/data/inevent/UEDIN_ASR_201407/$PREFIX*`
do 
	echo  $sentfile
	conv=`basename $sentfile .json`
	qsub -N get-aug-$conv -hold_jid get-tfpros-asrlex-$conv $SGESCRIPTS/get-ed-aug-lex-aggs.sh $conv $WTYPE $WDIR
done
