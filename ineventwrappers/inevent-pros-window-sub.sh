#!/bin/bash

SGEDIR=../sgescripts/
CORPUS=inevent
DATADIR=$HOME/data/$CORPUS/derived/
SEGSDIR=$DATADIR/segs/

WTYPE=$1
FEATNAME=$2
CONV=$3
WDIR=$4

file=$SEGSDIR/$WTYPE/$CONV.conv.$WTYPE
echo $CONV $FEATNAME $SEGSDIR $WTYPE
qsub -N get-pwin-$FEATNAME-$WTYPE-$CONV -hold_jid get-pros-norm-$FEATNAME-$CONV,get-auto-sent-$CONV $SGEDIR/get-ed-pros-window.sh $CONV $FEATNAME $SEGSDIR $WDIR $WTYPE  
