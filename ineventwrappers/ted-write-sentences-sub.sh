#!/bin/bash

SGEDIR=../sgescripts/

transdir="$HOME/data/ted/transcripts/stm-ted2013/"
infofile="$HOME/data/ted/transcripts/id_dname_year_sname"
sentdir="$HOME/data/ted/traintrans/"
testdir="$HOME/data/ted/testtrans/"
excludefile="$HOME/data/ted/inevent-ted.txt"

echo "*** get-spurt-feats start ***"

if [ ! -e $sentdir ]
then
        mkdir $sentdir
fi


for file in $transdir/*.stm
do
	fstem=`basename $file .stm`
	if [[ `grep TED${fstem} $excludefile | wc -w` -eq 0 ]] 
	then
		echo $file  $fstem
		#qsub $SGEDIR/get-ed-write-sentences.sh $fstem.stm $transdir $infofile $sentdir
	else 
		echo exclude: $fstem
		qsub $SGEDIR/get-ed-write-sentences.sh $fstem.stm $transdir $infofile $testdir
	fi
	


done



