#!/bin/bash

PREFIX=$1
datadir=~/data/$CORPUS/derived/

for file in `ls ~/data/inevent/UEDIN_ASR_201407/$PREFIX*`
do 
	conv=`basename $file .json`
	echo  $conv
	## Prosody aggregates over different sentence segmentations 
	## Based on full-stop insertion
	segsdir=$datadir/segs/asrsent/
	./inevent-pros-window-sub.sh asrsent f0 $conv $segsdir
	./inevent-pros-window-sub.sh asrsent i0 $conv $segsdir

	## Deleting lower confidence words 
	segsdir=$datadir/segs/asrsent/
	./inevent-pros-window-sub.sh "autosent0.7" f0 $conv $segsdir
	./inevent-pros-window-sub.sh "autosent0.7" i0 $conv $segsdir

done
