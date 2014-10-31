#!/bin/bash
# Get features for a single
#----------------------------------------------------
NNDIR=../nn/
RSCRIPTS=../rscripts/
SGEDIR=../sgescripts/
SCRIPTDIR=../ineventwrappers/

## Current conv
conv=$1
varname=$2
windowtype=$3 	# asrutt
windowdir=$4
featnames=$5

## inevent vars 
corpus=inevent
segsdir=$HOME/data/inevent/derived/segs/
hashead=F
lexname=asrlex
spkonly=F
fsuffix=".tf.allpros.txt"
normtype="all"
skip=0		# If feature files have a header
lextype=asr
testonly=T

## Current features
pcontext=0
ncontext=0

## Determine number of input features
n_step=$(($pcontext + $ncontext + 1))
nfeatnames=$((`echo $featnames | wc -w` - 1))
n_in=$(($n_step * $nfeatnames))
## Including delta features
if [ $(($n_step - 1)) -gt 0 ]
then
        n_in=$(($n_in * 2))
fi
echo $n_in

featname=$conv.$varname-$pcontext.$ncontext.$normtype

includehead=F

# trained model stem
trainmlpdir="/exports/home/clai/nn/output/mlp/"
traincorpus="ami"
trainpkl=${traincorpus}_mlp_${varname}-${pcontext}.${ncontext}.${normtype}
echo $trainpkl

# get the best performing mlp model parameters from dev experiments 
trainmlpfile=`head -n1 ${trainmlpdir}/plain/${trainpkl}.auroc.txt | cut -d " " -f 2 | sed s/.dev.txt//g`
scale=`head -n1 ${trainmlpdir}/plain/${trainpkl}.auroc.txt | cut -d " " -f 4`

hsize=$(($n_in * $scale))
hname=`echo $hsize | tr " " "_"`

fset=$featname
#-----------------------------------------------------------------------
# get longer segment, e.g. DA, level features 

echo "--- get word da features ---"
fset_layer=${fset}_${n_in}-${hsize}
SPKONLY=T

echo $windowdir

echo ${fset_layer}
Rscript $RSCRIPTS/get-tf-window.r $conv $lexname $segsdir $windowtype $SPKONLY ${fset_layer} $windowdir 

echo "aggs DONE"
#exit 0

