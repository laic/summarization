#!/bin/bash

## Only get DAs for convs that have extractive summaries 
EXTRACTS="$HOME/data/ami/Data/AMI/NXT-format/extractive/"

OUTDIR=$1

for file in `ls $EXTRACTS/*ext*.xml`  
do 
	fstem=`basename $file | cut -d "." -f 1`
	echo $fstem
	./ami-da-obs.sh $fstem | ./nxt-clean.sh > $OUTDIR/$fstem.da.txt  
done 


