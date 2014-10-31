#!/bin/bash

SGESCRIPTS="../sgescripts"

corpus=inevent
datadir=/exports/home/clai/data/inevent/derived/
pscores=F
lextype=asrlex 

fsetname=$1	## the feature set designation, this is hardcoded for the moment in the R file 
prefix=$2	## e.g. the conversion name, conv
wtype=$3

echo "********************************"
echo "Name: get-fx0-$fsetname-$prefix"
echo "holds: get-aug-$prefix,get-tfseq-i0-$prefix,get-tfseq-f0-$prefix"

qsub -N get-fx0-$fsetname-$prefix -hold_jid get-aug-$prefix,get-tfseq-i0-$prefix,get-tfseq-f0-$prefix  $SGESCRIPTS/get-ed-fx0.sh $fsetname $corpus $datadir $pscores $lextype $prefix $wtype

