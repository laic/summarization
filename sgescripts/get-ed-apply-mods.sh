#!/bin/bash

#$ -N get-da-mods
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=/disk/data1/clai/local/bin:~/local/bin/:$PATH
RSCRIPTS="../rscripts/"

# Run the program
fsetname=$1
dataset=$2
corpus=$3
datadir=$4 # ~/data/ami/derived/
moddataset=$5
moddatadir=$6
modcorpus=$7
wordfile=$8
wtype=$9

Rscript $RSCRIPTS/apply-da-mods.r $fsetname $dataset $corpus $datadir $moddataset $moddatadir $modcorpus $wordfile $wtype


