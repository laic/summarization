#!/bin/bash

#$ -N write-sentences
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=~/local/bin:$PATH

# Run the program
# -pe memory-2G 2

filename=$1
transdir=$2
infofile=$3
sentdir=$4

Rscript ../rscripts/write-sentences.r $filename $transdir $infofile $sentdir 


