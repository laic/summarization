#!/bin/bash

#$ -N get-stop-mod 
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

module load java
export PATH=~/local/bin:$PATH
#export _JAVA_OPTIONS="-Xmx1g"
export _JAVA_OPTIONS="-Xmx100m"

# Run the program
# -pe memory-2G 2
#transdir=$HOME/data/ted/traintrans/
#filename=$transdir/"0182.stm.sentences.txt"

filename=$1
posdir=$2
echo $filename $posdir

Rscript ../sentence_breaks/get-pos-conv.r $filename $posdir


