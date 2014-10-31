#!/bin/bash

#$ -N apply-punc-mod
#$ -pe memory-2G 2
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=~/local/bin:$PATH

# Run the program

filename=$1
punctree=$2
wordvar=$3
startvar=$4
wordid=$5

Rscript ../sentence_breaks/apply-punc-mod.r $filename $punctree $wordvar $startvar $wordid


