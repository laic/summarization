#!/bin/bash
# Get features for a single
#----------------------------------------------------
NNDIR=../nn/
RSCRIPTS=../rscripts/
SGEDIR=../sgescripts/

## Current conv
conv=$1
varname=$2
featnames=$3

## inevent  vars 
segsdir=$HOME/data/inevent/derived/segs/
hashead=F
corpus=inevent
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

trainmlpdir=$NNDIR/output/mlp/
if [ ! -e $trainmlpdir ]
then
        echo "mkdir $trainmlpdir" 
fi

mlpprobdir=$NNDIR/output/mlp/plain/
if [ ! -e $mlpprobdir ]
then
        echo "mkdir $mlpprobdir exists" 
fi

mlpdatadir=$NNDIR/data/$corpus/
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

## HERE!!!!
 
#cp $testsetdir/$featname.txt  $nnrawdir/$featname-eval.txt

cp $testsetdir/$featname.txt  $testsetdir/$featname-eval.txt

## Put data in a format good for theano/MLP   
## This is a bit superfluous in this case, we might as well just directly read in 
## when applying the mlp

#echo $featname $nnrawdir $corpus $skip

#if [ -e $nnrawdir/$featname.pkl.gz ]
#then
#        rm $nnrawdir/$featname.pkl.gz
#fi

#echo "proc vec"
#python ~/scripts/proc_vecs.py $featname $nnrawdir $skip $testonly

#echo "gzip"
#echo $nnrawdir/$featname.pkl
#
#gzip $nnrawdir/$featname.pkl
#cp $nnrawdir/$featname.pkl.gz $mlpdatadir
#
##----------------------------------------------------------------
## Apply the ami model
##----------------------------------------------------------------
echo "--- Apply the ami model ---"

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

model=$trainmlpdir/params-$trainmlpfile
infile=$testsetdir/$featname-eval.txt
outfile=$testsetdir/${corpus}_mlp_${featname}_${n_in}-$hname.prob

echo $infile $n_in $corpus $testonly $model $hsize
./apply-mlp.sh $infile $model $n_in $hsize $outfile $testonly

##-----------------------------------------------------------------------
## get flat file version of mlp output
echo $teststem
#./get-mlp-out.sh $teststem $n_in $segsdir "$scale"
#

# Join back with identifiers, niteid etc.
probfname=$testsetdir/${corpus}_mlp_${fset}_${n_in}-${hsize}.prob.eval.txt
idfname=$testsetdir/$featname-eval.txt
lexdir=$segsdir/${lextype}lex/
Rscript $RSCRIPTS/get-word-ids.r $probfname $idfname $lexdir


##-----------------------------------------------------------------------
## get DA level features 
#echo "--- get word da features ---"
#fset_layer=${fset}_${n_in}-${hsize}
#wtype=asrutt
#SPKONLY=T
#
#echo ${fset_layer}
#Rscript ~/scripts/get-tf-window.r $conv $lexname $segsdir $wtype $SPKONLY ${fset_layer} #> $PROSPATH/$feat/$conv.log  
#
#echo "DONE"
#exit 0
#
