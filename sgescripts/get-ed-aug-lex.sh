#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=36:00:00
# Initialise environment module

. /etc/profile.d/modules.sh


# Run the program
CONV=$1

export PATH=~/local/bin:$PATH 

echo "*** get augmented lexical features ***"
SCRIPTDIR=../ineventwrappers/

echo "--- tf-if-slope ---"
varname="tf-if-slope"
featnames="niteid tf.idf mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"

. $SCRIPTDIR/inevent-aug-lex.sh $CONV $varname "$featnames"

#
#echo "--- tfsp-if-slope ---"
#varname="tfsp-if-slope"
#featnames="niteid tf.idf su.idf pmi_f pmi_t mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
#./inevent-aug-lex.sh $CONV $varname "$featnames"
#
#echo "--- if ---"
#varname="if"
#featnames="niteid mean.normF0.slope sd.normF0.slope q2.5.normF0.slope q97.5.normF0.slope mean.normI0.slope sd.normI0.slope q2.5.normI0.slope q97.5.normI0.slope"
#./inevent-aug-lex.sh $CONV $varname "$featnames"

echo "*** finished get-aug-$conv ***"



