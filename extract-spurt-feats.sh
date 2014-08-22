#!/bin/bash

#spurtfile=~/data/ami/derived/ami.spurts.txt
#spurtfile=~/data/ami/derived/ami.asr.spurts.txt
#spurtfile=~/data/ted/derived/ted.utts.txt.test
phtools=/exports/home/clai/clai/phtools/
PRAAT=/exports/home/clai/.local/bin/praat
spurtfile=$1
spurtdir=$2 	## This is the output directory 	
#indir="/exports/home/clai/data/ted/wav/"
indir=$3	## i.e. where the wav files are    

cat $spurtfile |
while read line
do
	conv=`echo $line | cut -d " " -f 1`
	spk=`echo $line | cut -d " " -f 2`
	part=`echo $line | cut -d " " -f 3`
	sid=`echo $line | cut -d " " -f 4`
	chno=`echo $line | cut -d " " -f 5`
	vidsrc=`echo $line | cut -d " " -f 6`
	start=`echo $line | cut -d " " -f 7`
	end=`echo $line | cut -d " " -f 8`
	niteid=`echo $line | cut -d " " -f 9`
	vconv=`echo $line | cut -d " " -f 10`

	if [[ $chno != "NA" ]] 
	then
		echo "multiple channels?" 
	fi

	if [[ $vidsrc == "ted" ]]	
	then 
		segfile=${vconv}-light.wav
	else  
		segfile=${vconv}.wav
		#echo "unknown video source:" $vidsrc	
	fi

        #outfile=${conv}_${spk}_${part}_${sid}
	outfile=$niteid
	echo "$segfile $outfile $start $end" #$indir $outdir"
	outdir="$spurtdir/$conv/"
	echo $indir
	echo $outdir

        if [ -e $outdir ]
        then
                echo "$conv exists" 
        else
                mkdir $outdir
                mkdir $outdir/$conv-wav 
                mkdir $outdir/$conv-f0
                mkdir $outdir/$conv-int
        fi

	echo  $segfile $outfile $start $end $indir $outdir $conv 
	$PRAAT $phtools/extract-feats.praat $segfile $outfile $start $end $indir $outdir $conv 

done  

exit 0
