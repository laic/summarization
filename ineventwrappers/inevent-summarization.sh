#!/bin/bash

## Input vars: e.g. TED0069 /disk/data3/inevent/1/
asrjson=$1
wavfile=$2
vidfile=$3
EVENTDIR=$4
conv=`basename $asrjson .asr.json`

echo $asrjson $EVENTDIR $conv

## Setup for inevent on majestic 

SCRIPTDIR=/disk/data1/clai/work/inevent/summarization/
INEVENTSCRIPTS=$SCRIPTDIR/ineventwrappers/
SGESCRIPTS=$SCRIPTDIR/sgescripts/
RSCRIPTS=$SCRIPTDIR/rscripts/
PRAATSCRIPTs=$SCRIPTDIR/praatscripts/
CORPUS="inevent"

datadir=$EVENTDIR/derived/
wavdir=$EVENTDIR/
#filename=$EVENTDIR/$conv.json
#infofile=~/data/$CORPUS/filenames.txt

if [ ! -e $datadir ]
then
	mkdir $datadir
fi

## The naming of these directories is a bit historical at this point ##
#$datadir/segs is where features related to various segments of the meeting ##
##are stored.  
if [ ! -e $datadir/segs ]
then
	mkdir $datadir/segs
	mkdir $datadir/segs/asrlex	## Lexical features	
	mkdir $datadir/segs/f0		## F0
	mkdir $datadir/segs/i0		## intensity
	mkdir $datadir/segs/reval	## Last level of R output: Utt level extractive summary probabilites etc.
	mkdir $datadir/segs/nn		## This is probably one to do away with
	mkdir $datadir/segs/all		## This one might be done away with too
	mkdir $datadir/segs/test 	## And this one
fi


## This is where raw features from praat end up.
## Rename to rawpros or something more descriptive?
if [ ! -e $datadir/wavutts ]
then
	mkdir $datadir/wavutts
	ln -s $datadir/wavutts $datadir/segs/conv
fi

## Timing info about words based on ASR output 
if [ ! -e $datadir/asrword ]
then
	mkdir $datadir/asrword
	ln -s $datadir/asrword $datadir/segs/
fi


## Timing info about utterances based on ASR output 
if [ ! -e $datadir/asrutt ]
then
	mkdir $datadir/asrutt
	ln -s $datadir/asrutt $datadir/segs/
fi

if [ ! -e $datadir/asrsent ]
then
	mkdir $datadir/asrsent
	ln -s $datadir/asrsent $datadir/segs/
fi

## This is for doing rouge stuff.  Not really needed if we don't have a gold
## Standard to compare to.

if [ ! -e $datadir/summeval/ ]
then
	mkdir $datadir/summeval/
	mkdir $datadir/summeval/systems
	mkdir $datadir/summeval/models
	echo "mkdir $datadir/summeval/"
fi

## Where we put our collated feature sets for DA/utterance level 
## predictions.
if [ ! -e $datadir/da-feats ]
then
	mkdir $datadir/da-feats
fi


#====================================================================================
## make infofile
echo "id video.file json.file wav.file" > $datadir/$conv.info.txt  
echo "x `basename $vidfile` `basename $asrjson` `basename $wavfile | sed s/[_-]//g`" >> $datadir/$conv.info.txt
cat $datadir/$conv.info.txt | sed 's/ /\t/g' > $datadir/info.txt
mv $datadir/info.txt $datadir/$conv.info.txt
infofile=$datadir/$conv.info.txt
## Get lexical word and utterance timings and calculate  lexical features 
## sge: get-new-json-$conv,  no holds 

echo $SGESCRIPTS
qsub -N get-new-json-$conv $SGESCRIPTS/get-ed-new-json.sh $asrjson $CORPUS $infofile  

wordfile=$datadir/asrword/$conv.raw.asrword.txt
punctreefile=../sentence/punctree
wordvar=word
startvar=wstart
wordid=niteid

#$SGESCRIPTS/get-ed-apply-punc.sh $wordfile $punctreefile $wordvar $startvar $wordid
qsub -N get-autopunc-$conv -hold_jid get-new-json-$conv $SGESCRIPTS/get-ed-apply-punc.sh $wordfile $punctreefile $wordvar $startvar $wordid

sentfile=$datadir/asrword/$conv.autopunc.words.txt
qsub -N get-auto-sent-$conv -hold_jid get-autopunc-$conv $SGESCRIPTS/get-ed-auto-sent.sh $sentfile $datadir
#$SGESCRIPTS/get-ed-auto-sent.sh $sentfile $datadir


qsub -N get-tf-feats-$conv -hold_jid get-new-json-$conv $SGESCRIPTS/get-ed-tf-feats.sh $conv $CORPUS $datadir

#====================================================================================
## sge: -N get-spurt-feats-$PREFIX -hold_jid get-new-json-$PREFIX
## where PREFIX=$conv
./inevent-pros-raw-sub.sh $conv $wavdir $datadir 

## Collate and normalize prosodic features  
## Outputs one file per conv and feature, e.g. ~/data/inevent/derived/segs/f0/TED0069 


## sge: -N get-pros-norm-$FEATNAME-$PREFIX -hold_jid get-spurt-feats-$PREFIX
## FEATNAME=f0, PREFIX=$conv
./inevent-pros-norm-sub.sh f0 $conv $datadir
### FEATNAME=i0, PREFIX=$conv
./inevent-pros-norm-sub.sh i0 $conv $datadir

##------------------------------------------------------------
## segsdir is the working directory for collecting aggregate features
segsdir=$datadir/segs/

## Word level prosodic aggregates
## sge: -N get-pwin-$FEATNAME-$WTYPE-$PREFIX -hold_jid get-pros-norm-$FEATNAME-$PREFIX 
wdir=$datadir/segs/asrword/
## window type, feature, conv, window directory, working directory
./inevent-pros-window-sub.sh asrword f0 $conv $wdir $segsdir
./inevent-pros-window-sub.sh asrword i0 $conv $wdir $segsdir

### Utterance level prosodic aggregates
### sge: -N get-pwin-$FEATNAME-$WTYPE-$PREFIX -hold_jid get-pros-norm-$FEATNAME-$PREFIX 
### FEATNAME={f0,i0}, WTYPE=asrutt, PREFIX=$conv 
wdir=$datadir/segs/asrutt/
./inevent-pros-window-sub.sh asrutt f0 $conv $wdir $segsdir
./inevent-pros-window-sub.sh asrutt i0 $conv $wdir $segsdir

### Prosody aggregates over different sentence segmentations 
### Based on full-stop insertion
wdir=$datadir/segs/asrsent/
./inevent-pros-window-sub.sh asrsent f0 $conv $wdir $segsdir
./inevent-pros-window-sub.sh asrsent i0 $conv $wdir $segsdir

## Deleting lower confidence words 
wdir=$datadir/segs/asrsent/
./inevent-pros-window-sub.sh "autosent0.7" f0 $conv $wdir $segsdir
./inevent-pros-window-sub.sh "autosent0.7" i0 $conv $wdir $segsdir

#------------------------------------------------------------
## combine term-frequency and prosodic word features
## sge: -N get-tfpros-$wtype{lex}-$conv -hold_jid get-tf-feats-$conv,get-pwin-i0-${wtype}lex-$prefix,get-pwin-f0-${wtype}lex-$prefix
## wtype=asr, prefix=$conv 
wfile="$datadir/segs/asrword/$conv.raw.asrword.txt"
./inevent-tf-pros-all-sub.sh asr $conv $wfile $segsdir

## Get augmented lexical features
#echo "Name: get-aug-$conv"
#echo "holds: get-tfpros-asrlex-$conv"

qsub -N get-aug-$conv -hold_jid get-tfpros-asrlex-$conv $SGESCRIPTS/get-ed-aug-lex.sh $conv $segsdir

## Utterance level prosody delta features 
# -N get-tfseq-$featname-$conv -hold_jid get-pwin-$featname-asrutt-$conv
## $featname={i0,f0} 
#fsuffix=".aggs.asrutt.txt"
wtype=asrutt
./inevent-pros-da-aggs.sh $conv $wtype $segsdir

#------------------------------------------------
#
## Gather lexical features for utterance level prediction 
## -N get-fx0-$fsetname-$prefix -hold_jid get-aug-$prefix,get-tfseq-i0-$prefix,get-tfseq-f0-$prefix
./inevent-lex-feats-sub.sh "aug.wsw" $conv asrutt $datadir

## Apply AMI DA models
## -N apply-mod-$dataset -hold_jid get-fx0-$fsetname-$conv  
## fsetname="aug.wsw"
moddir=/disk/data1/clai/work/inevent/data/ami/derived/
./inevent-apply-mods-sub.sh "aug.wsw" "$conv.$CORPUS.group.fx0.aug.wsw.asrutt" "ami.group.fx0.aug.wsw" asrutt $datadir $moddir 

wtype="autosent0.7"
./inevent-pros-da-aggs.sh $conv $wtype $segsdir
./inevent-lex-feats-sub.sh "aug.wsw" $conv "autosent0.7" $datadir
./inevent-apply-mods-sub.sh "aug.wsw" "$conv.$CORPUS.group.fx0.aug.wsw.autosent0.7" "ami.group.fx0.aug.wsw" "autosent0.7" $datadir $moddir 

#./inevent-apply-mods-sub.sh "da.bare" "$conv.$CORPUS.group.fx0.aug.wsw" "ami.group.fx0.aug.wsw"
#cp $segsdir/reval/ami.group.fx0.aug.wsw/$conv.tf.pros_pros.asrutt.json $EVENTDIR/$conv.asrutt.extsumm.json
cp $segsdir/reval/ami.group.fx0.aug.wsw/$conv.tf.pros_pros.autosent0.7.json $EVENTDIR/$conv.quotes.json

