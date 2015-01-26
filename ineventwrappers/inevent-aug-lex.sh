#!/bin/bash
# Get features for a single
#----------------------------------------------------
#NNDIR=../nn/
RSCRIPTS=../rscripts/
SGEDIR=../sgescripts/
SCRIPTDIR=../ineventwrappers/

## Current conv
conv=$1
varname=$2
segsdir=$3
nnoutdir=$4
featnames=$5
#segsdir=$HOME/data/inevent/derived/segs/


## inevent  vars 
corpus=inevent
hashead=F
lexname=asrlex
spkonly=F
fsuffix=".tf.allpros.txt"
normtype="all"
skip=0		# If feature files have a header
lextype=asr
testonly=T

if [ `ls $segsdir/$lexname/$conv*$fsuffix | wc -w` -lt 1 ] 
then 
	echo "No $conv tf.allpros.txt file in $segsdir/$lexname/. Exiting..."   
	exit 1
else 
	echo `ls $segsdir/$lexname/$conv*$fsuffix`
fi


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

#----------------------------------------------------
# Make directories
#----------------------------------------------------

#trainmlpdir=$nnoutdir/output/mlp/
#if [ ! -e $trainmlpdir ]
#then
#        echo "mkdir $trainmlpdir" 
#fi

mlpprobdir=$nnoutdir/output/mlp/plain/
if [ ! -e $mlpprobdir ]
then
        echo "mkdir $mlpprobdir exists" 
fi

mlpdatadir=$nnoutdir/data/$corpus/
if [ ! -e $mlpdatadir ]
then
        echo "mkdir $mlpdatadir" 
fi

nnrawdir=$segsdir/nn/ 
if [ ! -e $nnrawdir ]
then
        echo "mkdir $nnrawdir" 
fi

testsetdir=$segsdir/test/ 
if [ ! -e $testsetdir ]
then
        echo "mkdir $testsetdir" 
fi

alldir=$segsdir/all/ 
if [ ! -e $alldir ]
then
        echo "mkdir $alldir" 
fi

#----------------------------------------------------
# Unroll everything
#----------------------------------------------------

#./get-tf-seq-sub.sh -x "*2008" $lexname $varname ted "$featnames" 
includehead=F
Rscript $RSCRIPTS/get-tf-seq.r $conv $lexname $segsdir $spkonly $pcontext $ncontext $varname $includehead $corpus $fsuffix $featnames
cp $testsetdir/$featname.txt  $testsetdir/$featname-eval.txt

##----------------------------------------------------------------
## Apply the ami model
##----------------------------------------------------------------
echo "--- Apply the ami model ---"

# trained model stem
#trainmlpdir="/exports/home/clai/nn/output/mlp/"
trainmlpdir=/disk/data1/clai/work/inevent/data/mlp/
traincorpus="ami"
trainpkl=${traincorpus}_mlp_${varname}-${pcontext}.${ncontext}.${normtype}
echo $trainpkl

# get the best performing mlp model parameters from dev experiments 
trainmlpfile=`head -n1 ${trainmlpdir}/plain/${trainpkl}.auroc.txt | cut -d " " -f 2 | sed s/.dev.txt//g`
scale=`head -n1 ${trainmlpdir}/plain/${trainpkl}.auroc.txt | cut -d " " -f 4`

hsize=$(($n_in * $scale))
hname=`echo $hsize | tr " " "_"`

fset=$featname

model=$trainmlpdir/params-$trainmlpfile
infile=$testsetdir/$featname-eval.txt
outfile=$testsetdir/${corpus}_mlp_${featname}_${n_in}-$hname.prob

echo $infile $n_in $corpus $testonly $model $hsize
. $SCRIPTDIR/apply-mlp.sh $infile $model $n_in $hsize $outfile $testonly

echo "------------------------------------"
#-----------------------------------------------------------------------
## get flat file version of mlp output
echo $teststem


# Join back with identifiers, niteid etc.

echo "---- get-word-ids ---------" 
probfname=$testsetdir/${corpus}_mlp_${fset}_${n_in}-${hsize}.prob.eval.txt
idfname=$testsetdir/$featname-eval.txt
lexdir=$segsdir/${lextype}lex/
Rscript $RSCRIPTS/get-word-ids.r $probfname $idfname $lexdir


#-----------------------------------------------------------------------
# get longer segment, e.g. DA, level features 

echo "--- get word da features ---"
fset_layer=${fset}_${n_in}-${hsize}
windowtype=asrutt
SPKONLY=T

echo ${fset_layer}
Rscript $RSCRIPTS/get-tf-window.r $conv $lexname $segsdir $windowtype $SPKONLY ${fset_layer} $segsdir/asrutt 

echo "autosent0.7"
windowtype=autosent0.7
Rscript $RSCRIPTS/get-tf-window.r $conv $lexname $segsdir $windowtype $SPKONLY ${fset_layer} $segsdir/asrsent/  

echo "DONE"

