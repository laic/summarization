#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=6:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=/disk/data1/clai/local/bin:$HOME/local/bin:$PATH

RSCRIPTS=../rscripts/
#module load R
# Run the program

CONV=$1
FEAT=$2
SEGSDIR=$3
WDIR=$4
WTYPE=$5

Rscript $RSCRIPTS/get-pros-window.r $CONV $FEAT $SEGSDIR $WDIR $WTYPE 

