#!/bin/bash

#SGESCRIPTS="../sgescripts/"

fsetname=$1
dataset=$2
corpus=inevent
datadir=/exports/home/clai/data/inevent/derived/
moddataset=$3 # "ami.group.fx0.wsw"
moddatadir=/exports/home/clai/data/ami/derived/
modcorpus=ami

conv=`echo $dataset | cut -d "." -f1`
dawordfile=$datadir/asrword/$conv.raw.asrword.txt

dset=`echo $dataset | sed s/.fx0./-/g | cut -d "-" -f2`
echo "***************************"
echo $dset 

echo $fsetname $dataset $corpus $datadir $moddataset $moddatadir $dawordfile
echo "Name: apply-mod-$dataset" 
echo "holds: get-fx0-$dset-$conv"

qsub -N apply-mod-$dataset -hold_jid get-fx0-$dset-$conv $SGESCRIPTS/get-ed-apply-mods.sh $fsetname $dataset $corpus $datadir $moddataset $moddatadir $modcorpus $dawordfile

