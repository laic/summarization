#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=/disk/data1/clai/local/bin/:$HOME/local/bin/:$PATH
RSCRIPTS="../rscripts/"

#module load R
# Run the program

conv=$1
lexname=$2
spkonly=$3
pwindow=$4
nwindow=$5
varname=$6  		#tfonly.txt
includehead=$7
corpus=$8
datadir=$9 		#/exports/home/clai/data/ami/derived/segs/
fsuffix=${10} 		#.tf.pros.all?
featnames="${11}" 	#"niteid tf.idf su.idf"

echo $featnames

Rscript $RSCRIPTS/get-tf-seq.r $conv $lexname $datadir $spkonly $pwindow $nwindow $varname $includehead $corpus $fsuffix $featnames


