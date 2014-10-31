#!/bin/bash

SGEDIR=../sgescripts/

PREFIX=$1
wavdir=$2
datadir=$3

segdir=$datadir/asrutt/
spurtdir=$datadir/wavutts/
spurttype="asrspurts"


echo "*** get-spurt-feats start ***"

if [ ! -e $spurtdir ]
then
        mkdir $spurtdir
fi

if [ ! -e $datadir/segs/conv ]
then
        ln -s  $spurtdir $datadir/segs/conv
fi

file=$segdir/$PREFIX.$spurttype.txt
echo $file $spurtdir $wavdir
echo "Name: get-spurt-feats-$PREFIX"
echo "hold: get-new-json-$PREFIX"
qsub -N get-spurt-feats-$PREFIX -hold_jid get-new-json-$PREFIX  $SGEDIR/get-ed-spurt-feats.sh $file $spurtdir $wavdir
#$SGEDIR/get-ed-spurt-feats.sh $file $spurtdir $wavdir





