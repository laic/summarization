#!/bin/bash

SGEDIR=../sgescripts/

datadir=~/data/
corpus=inevent
segsdir=$datadir/$corpus/derived/segs/
wprefix=""
rmstop=T	# set stopwords tf.idf and su.idf values to zero

wtype="$1"	# asr,  manual  
conv=$2		 
echo $conv $wtype

if [ "$wtype" != "manual" ]
then
	wprefix=$wtype
	echo "prefix: $wprefix"
fi		

file=$segsdir/${wprefix}lex/${conv}.lex.grp
i0wordjid=get-pwin-i0-${wtype}word-$conv
f0wordjid=get-pwin-f0-${wtype}word-$conv

echo "Name: get-tfpros-${wtype}lex-$conv"
echo "holds: $i0wordjid $f0wordjid"

qsub -N get-tfpros-${wtype}lex-$conv -hold_jid $i0wordjid,$f0wordjid  $SGEDIR/get-ed-tf-pros-all.sh $conv $segsdir $rmstop $wtype
