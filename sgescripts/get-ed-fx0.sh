#!/bin/bash

#$ -N group.fx0
#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=~/local/bin/:$PATH
RSCRIPTS="../rscripts/"
#module load R
# Run the program
# -pe memory-2G 2

fsetname=$1	## Name of the feature set we want collate, e.g. "aug.wsw" = augmented with stop words 
corpus=$2	## e.g. inevent
datadir=$3
pscores=$4 	## Include participation measures?
lextype=$5	## i.e. asr 
prefix=$6	## i.e. conv
wtype=$7	## e.g. asrsent


echo "R: `which R`"
echo $fsetname $corpus $datadir $pscores $lextype $prefix

Rscript $RSCRIPTS/get-lex-feats.r $fsetname $corpus $datadir $pscores $lextype $prefix $wtype


