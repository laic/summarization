#!/bin/bash
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=/disk/data1/clai/local/bin:$HOME/local/bin:$PATH

RSCRIPTS=../rscripts/
# Run the program

conv=$1
segsdir=$2
rmstop=$3
wtype="$4"
segfile="$5"

Rscript $RSCRIPTS/get-word-tf-pros.r $conv lex $segsdir $rmstop $wtype $segfile

