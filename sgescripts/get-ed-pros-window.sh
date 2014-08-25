#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=24:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=$HOME/local/bin:$PATH

RSCRIPTS=../rscripts/
#module load R
# Run the program

CONV=$1
FEAT=$2
SEGSDIR=$3
WTYPE=$4

Rscript $RSCRIPTS/get-pros-window.r $CONV $FEAT $SEGSDIR $SEGSDIR/$WTYPE $WTYPE 

