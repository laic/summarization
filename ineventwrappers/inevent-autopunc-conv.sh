#!/bin/bash

SGESCRIPTS="../sgescripts/"
RSCRIPTS="../rscripts/"
PRAATSCRIPTs="../praatscripts/"
CORPUS="inevent"

conv=$1

## Setup for inevent on eddie 
datadir=~/data/$CORPUS/derived/
filename=~/data/$CORPUS/UEDIN_ASR_201407/$conv.json
infofile=~/data/$CORPUS/filenames.txt
echo $conv $filename

wordfile=$datadir/asrword/$conv.raw.asrword.txt
punctreefile=../sentence/punctree
wordvar=word
startvar=wstart
wordid=niteid
qsub -N get-autopunc-$conv -hold_jid get-new-json-$conv $SGESCRIPTS/get-ed-apply-punc.sh $wordfile $punctreefile $wordvar $startvar $wordid

sentfile=$datadir/asrword/$conv.autopunc.words.txt
qsub -N get-auto-sent-$conv -hold_jid get-autopunc-$conv $SGESCRIPTS/get-ed-auto-sent.sh $sentfile $datadir


