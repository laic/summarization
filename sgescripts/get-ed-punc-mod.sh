#!/bin/bash

#$ -N get-punc-mod
#$ -pe memory-2G 2
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

#module load java
export PATH=~/local/bin:$PATH
#export _JAVA_OPTIONS="-Xmx1g"
#export _JAVA_OPTIONS="-Xmx100m"

# Run the program
#transdir=$HOME/data/ted/traintrans/
#filename=$transdir/"0182.stm.sentences.txt"

posdir=$HOME/data/ted/trainpos/

Rscript ../sentence_breaks/get-c5-tree.r $posdir


