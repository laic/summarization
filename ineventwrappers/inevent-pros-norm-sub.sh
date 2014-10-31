#!/bin/bash
SGEDIR=../sgescripts/

FEATNAME=$1
CONV=$2
DATADIR=$3

CORPUS=inevent
#DATADIR=$HOME/data/$CORPUS/derived/
SEGSDIR=$DATADIR/segs/
#CONVDIR=$DATADIR/wavutts/
UTTDIR=$DATADIR/asrutt/

echo "*** get-pros-norm: $CONV ***"
#echo $CONVDIR

#d=$CONVDIR/$CONV/
#echo $d

SPURTFILE=$UTTDIR/$CONV.asrspurts.txt
echo $CONV $FEATNAME $SEGSDIR $SPURTFILE

echo "Name: get-pros-norm-$FEATNAME-$CONV"
echo "hold: get-spurt-feats-$CONV"
qsub -N get-pros-norm-$FEATNAME-$CONV -hold_jid get-spurt-feats-$CONV $SGEDIR/get-ed-pros-norm.sh $CONV $FEATNAME $SEGSDIR $SPURTFILE 

echo "*******************************"
