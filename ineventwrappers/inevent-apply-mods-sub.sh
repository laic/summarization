#!/bin/bash

#SGESCRIPTS="../sgescripts/"

fsetname=$1
dataset=$2
corpus=inevent
datadir=/exports/home/clai/data/inevent/derived/
moddataset=$3 # "ami.group.fx0.wsw"
moddatadir=/exports/home/clai/data/ami/derived/
modcorpus=ami
wtype=$4

conv=`echo $dataset | cut -d "." -f1`
echo wtype=$wtype
if [ "$wtype" == "asrutt" ]
then 
	wordfile=$datadir/asrword/$conv.raw.asrword.txt
else 
	wordfile=$datadir/asrsent/$conv.$wtype.trans.txt
fi


dset=`echo $dataset | sed s/.fx0./-/g | cut -d "-" -f2`
echo "***************************"
echo $dset 

echo $fsetname $dataset $corpus $datadir $moddataset $moddatadir $wordfile
echo "Name: apply-mod-$dataset" 
echo "holds: get-fx0-$dset-$conv"

qsub -N apply-mod-$dataset -hold_jid get-fx0-$dset-$conv $SGESCRIPTS/get-ed-apply-mods.sh $fsetname $dataset $corpus $datadir $moddataset $moddatadir $modcorpus $wordfile $wtype

#$SGESCRIPTS/get-ed-apply-mods.sh $fsetname $dataset $corpus $datadir $moddataset $moddatadir $modcorpus $wordfile $wtype
