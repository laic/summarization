#!/bin/bash

#$ -N get-da-mods
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=~/local/bin/:$PATH
RSCRIPTS="../rscripts/"
# Run the program
# -pe memory-2G 2

fsetname=$1
dataset=$2
corpus=$3
datadir=$4 # ~/data/ami/derived/
moddataset=$5
moddatadir=$6
modcorpus=$7
dawordfile=$8


Rscript $RSCRIPTS/apply-da-mods.r $fsetname $dataset $corpus $datadir $moddataset $moddatadir $modcorpus $dawordfile


