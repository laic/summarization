#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=6:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

SCRIPTDIR=../ineventwrappers/
export HOME=/disk/data1/clai/
export PATH=/disk/data1/clai/.local/bin/:disk/data1/clai/local/bin:~/local/bin:$PATH 
export THEANO_FLAGS='device=cpu,pycuda.init=False,gcc.cxxflags=-m64 -L/usr/lib64/atlas/ -L/usr/lib64/,blas.ldflags=-lf77blas -latlas -lgfortran,on_unused_input=ignore'
export PYTHONPATH=/disk/data1/clai/.local/lib/python2.6:$PYTHONPATH

# Run the program
CONV=$1
segsdir=$2
nnoutdir=$segsdir/nn

echo "*** get augmented lexical features ***"

echo "--- tf-if-slope ---"
varname="tf-if-slope"
featnames="niteid tf.idf mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"

. $SCRIPTDIR/inevent-aug-lex.sh $CONV $varname $segsdir $nnoutdir "$featnames"


#echo "--- tfsp-if-slope ---"
#varname="tfsp-if-slope"
#featnames="niteid tf.idf su.idf pmi_f pmi_t mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
#. $SCRIPTDIR/inevent-aug-lex.sh $CONV $varname  $segsdir $nnoutdir "$featnames"
#
#echo "--- if ---"
#varname="if"
#featnames="niteid mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
#. $SCRIPTDIR/inevent-aug-lex.sh $CONV $varname  $segsdir $nnoutdir "$featnames"

echo "*** finished get-aug-$conv ***"



