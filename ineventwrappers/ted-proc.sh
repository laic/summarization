#!/bin/bash
## json test run

SGESCRIPTS="../sgescripts/"
RSCRIPTS="../rscripts/"
PRAATSCRIPTs="../praatscripts/"
CORPUS="ted"

#filename=~/data/inevent/UEDIN_ASR_201407/TED0069.json
conv=$1

## Setup for inevent on eddie 
datadir=~/data/$CORPUS/derived/
filename=~/data/$CORPUS/UEDIN_ASR_201407/$conv.json
infofile=~/data/$CORPUS/filenames.txt
echo $conv $filename


## The naming of these directories is a bit historical at this point
## $datadir/segs is where features related to various segments of the meeting 
## are stored.  
if [ ! -e $datadir/segs ]
then
	mkdir $datadir/segs
	mkdir $datadir/segs/asrlex	## Lexical features	
	mkdir $datadir/segs/f0		## F0
	mkdir $datadir/segs/i0		## intensity
	mkdir $datadir/segs/reval	## Last level of R output: Utt level extractive summary probabilites etc.
	mkdir $datadir/segs/nn	## This is probably one to do away with
	mkdir $datadir/segs/all	## This one might be done away with too
	mkdir $datadir/segs/test ## And this one
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
## Get lexical word and utterance timings. In this case we  
## sge: get-new-json-$conv,  no holds 



## Calculate  lexical features 
#qsub -N get-new-json-$conv $SGESCRIPTS/get-ed-new-json.sh $filename  $CORPUS $infofile  


#=========================================================================================

qsub -N get-tf-feats-$conv -hold_jid get-new-json-$conv $SGESCRIPTS/get-ed-tf-feats.sh $conv $CORPUS

#====================================================================================
## sge: -N get-spurt-feats-$PREFIX -hold_jid get-new-json-$PREFIX
## where PREFIX=$conv
./inevent-pros-raw-sub.sh $conv 

## Collate and normalize prosodic features  
## Outputs one file per conv and feature, e.g. ~/data/inevent/derived/segs/f0/TED0069 


## sge: -N get-pros-norm-$FEATNAME-$PREFIX -hold_jid get-spurt-feats-$PREFIX
## FEATNAME=f0, PREFIX=$conv
./inevent-pros-norm-sub.sh f0 $conv	
## FEATNAME=i0, PREFIX=$conv
./inevent-pros-norm-sub.sh i0 $conv

#------------------------------------------------------------
## Word level prosodic aggregates
## sge: -N get-pwin-$FEATNAME-$WTYPE-$PREFIX -hold_jid get-pros-norm-$FEATNAME-$PREFIX 
./inevent-pros-window-sub.sh asrword f0 $conv
./inevent-pros-window-sub.sh asrword i0 $conv

## Utterance level prosodic aggregates
## sge: -N get-pwin-$FEATNAME-$WTYPE-$PREFIX -hold_jid get-pros-norm-$FEATNAME-$PREFIX 
## FEATNAME={f0,i0}, WTYPE=asrutt, PREFIX=$conv 
./inevent-pros-window-sub.sh asrutt f0 $conv
./inevent-pros-window-sub.sh asrutt i0 $conv

#------------------------------------------------------------
## combine term-frequency and prosodic word features
## sge: -N get-tfpros-$wtype{lex}-$conv -hold_jid get-tf-feats-$conv,get-pwin-i0-${wtype}lex-$prefix,get-pwin-f0-${wtype}lex-$prefix
## wtype=asr, prefix=$conv 
./inevent-tf-pros-all-sub.sh asr $conv

## Get augmented lexical features
echo "Name: get-aug-$conv"
echo "holds: get-tfpros-asrlex-$conv"

qsub -N get-aug-$conv -hold_jid get-tfpros-asrlex-$conv $SGESCRIPTS/get-ed-aug-lex.sh $conv

## Utterance level prosody delta features 
# -N get-tfseq-$featname-$conv -hold_jid get-pwin-$featname-asrutt-$conv
## $featname={i0,f0} 
./inevent-pros-da-aggs.sh $conv

#------------------------------------------------

## Gather lexical features for utterance level prediction 
## -N get-fx0-$fsetname-$prefix -hold_jid get-aug-$prefix,get-tfseq-i0-$prefix,get-tfseq-f0-$prefix

./inevent-lex-feats-sub.sh "aug.wsw" $conv 


## Apply AMI DA models
## -N apply-mod-$dataset -hold_jid get-fx0-$fsetname-$conv  
## fsetname="aug.wsw"

./inevent-apply-mods-sub.sh "aug.wsw" "$conv.$CORPUS.group.fx0.aug.wsw" "ami.group.fx0.aug.wsw"
./inevent-apply-mods-sub.sh "da.bare" "$conv.$CORPUS.group.fx0.aug.wsw" "ami.group.fx0.aug.wsw"


