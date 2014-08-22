#!/bin/bash

#phtools=/exports/home/clai/clai/phtools/
PRAAT=/exports/home/clai/.local/bin/praat
spurtfile=$1
spurtdir=$2 	## This is the output directory 	
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
		wavfile=${vconv}-light.wav
	else  
		wavfile=${vconv}.wav
		#echo "unknown video source:" $vidsrc	
	fi

	outfile=$niteid
	outdir="$spurtdir/$conv/"
	echo $indir
	echo $outdir

        if [ ! -e $outdir ]
        then
                mkdir $outdir
                mkdir $outdir/$conv-wav 
                mkdir $outdir/$conv-f0
                mkdir $outdir/$conv-int
        fi

	echo  $wavfile $outfile $start $end $indir $outdir $conv 
	$PRAAT ./extract-feats.praat $wavfile $outfile $start $end $indir $outdir $conv 

done  

exit 0
