#!/bin/bash

SGEDIR=../sgescripts/
ineventdir=/exports/home/clai/data/inevent/
datadir=$ineventdir/derived/
wavdir=$ineventdir/wav/
segdir=$datadir/asrutt/
spurtdir=$datadir/wavutts/
spurttype="asrspurts"

PREFIX=$1

echo "*** get-spurt-feats start ***"

if [ ! -e $spurtdir ]
then
        mkdir $spurtdir
fi

if [ ! -e $datadir/segs/conv ]
then
        ln -s  $spurtdir $datadir/segs/conv
fi

#for file in $segdir/$PREFIX*$spurttype.txt
#do
file=$segdir/$PREFIX.$spurttype.txt
echo $file $spurtdir $wavdir
echo "Name: get-spurt-feats-$PREFIX"
echo "hold: get-new-json-$PREFIX"
qsub -N get-spurt-feats-$PREFIX -hold_jid get-new-json-$PREFIX  $SGEDIR/get-ed-spurt-feats.sh $file $spurtdir $wavdir


#done



