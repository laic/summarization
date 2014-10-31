#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=6:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=/disk/data1/clai/local/bin/:~/local/bin:$PATH
#module load R
# Run the program

CONV=$1
FEATNAME=$2
SEGSDIR=$3
SPURTSFILE=$4

# -pe memory-2G 2

RSCRIPTS=../rscripts/

echo "PATH: $PATH"
Rscript $RSCRIPTS/get-pros-norm.r $CONV $FEATNAME $SEGSDIR $SPURTSFILE 



