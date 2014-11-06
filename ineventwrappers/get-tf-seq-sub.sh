#!/bin/bash

SGESCRIPTS="../sgescripts"

spkonly=F	
prevwindow=0
nextwindow=0
includehead=F			# include head movement velocities
fsuffix=".tf.allpros.txt"	# suffix of features files
datadir=/disk/data3/inevent/	# on eddie set directory to: exports/home/clai/data/inevent/derived/segs/
prefix=""			# Restrict the set we're working on


usage="-s spkonly\n-p #prev windows\n..."

while getopts "svx:p:n:f:d:" OPTION
do
     case $OPTION in
         s)
             spkonly=T 
             ;;
         p)
             prevwindow=$OPTARG
             ;;
         n)
             nextwindow=$OPTARG
             ;;
         v)
             includehead=T
             ;;
         f)
             fsuffix=$OPTARG
             ;;
         d)
             datadir=$OPTARG
             ;;
         x)
             prefix=$OPTARG
             ;;
	 *) 
  		echo "invalid arg" 
		exit
     esac
done

shift $(($OPTIND -1))
echo $fsuffix

featname=$1	# e.g. alignlex, lex, ...
varid=$2	# mnemonic for the feature set, e.g. tf-if-sw.	
corpus=$3 	# e.g. "ami"
varnames="$4"	# Features we're interested in

echo "...sub...."
echo $featname $varid $corpus
echo $varnames 	

file=$datadir/$featname/${prefix}${fsuffix}
echo $file
conv=`basename $file $fsuffix`
echo "Name: get-tfseq-$featname-$conv"
echo "holds: get-pwin-$featname-asrutt-$conv"
qsub -N get-tfseq-$featname-$conv -hold_jid get-pwin-$featname-asrutt-$conv  $SGESCRIPTS/get-ed-tf-seq.sh $conv $featname $spkonly $prevwindow $nextwindow $varid $includehead $corpus $datadir $fsuffix "$varnames"


