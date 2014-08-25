#!/bin/bash

#$ -N get-aug-feats 
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./tfwindows
#$ -e ./tfwindows
#$ -l h_rt=36:00:00
# Initialise environment module

. /etc/profile.d/modules.sh


#module load R
# Run the program

# -pe memory-2G 2
SCRIPTDIR=../ineventwrappers/
export PATH=~/local/bin:$PATH 

CONV=$1

echo "*** get augmented lexical features ***"

echo "--- tf-if-slope ---"
varname="tf-if-slope"
featnames="niteid tf.idf mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
$SCRIPTDIR/inevent-aug-lex.sh $CONV $varname "$featnames"

echo "--- tfsp-if-slope ---"
varname="tfsp-if-slope"
featnames="niteid tf.idf su.idf pmi_f pmi_t mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
$SCRIPTDIR/inevent-aug-lex.sh $CONV $varname "$featnames"

echo "--- if ---"
varname="if"
featnames="niteid mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
$SCRIPTDIR/inevent-aug-lex.sh $CONV $varname "$featnames"

echo "*** finished get-aug-$CONV ***"



