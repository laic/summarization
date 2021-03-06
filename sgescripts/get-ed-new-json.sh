#!/bin/bash

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
corpus=$2
infofile=$3
datadir=$4
#export DATADIR=$DATADIR
#export DATADIR="~/data/inevent/derived/"

echo `which R`

Rscript ../rscripts/get-new-json.r $filename $corpus $infofile $datadir


