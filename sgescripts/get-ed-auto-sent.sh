#!/bin/bash

#$ -N write-auto-sent
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=/disk/data1/clai/local/bin:~/local/bin:$PATH

# Run the program
# -pe memory-2G 2

filename=$1
datadir=$2

Rscript ../sentence_breaks/write-auto-sentences.r $filename $datadir


