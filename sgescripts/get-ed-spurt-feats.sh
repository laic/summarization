#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=$HOME/summarization/ineventwrappers/:$PATH

# Run the program


spurtfile=$1
spurtdir=$2 #~/data/ted/derived/wavutts/
indir=$3

echo $spurtfile $spurtdir $indir 

if [ ! -e $spurtfile ] 
then 
       echo "No spurt files $segdir/$PREFIX. Exiting" 
       exit 1  
fi

./extract-spurt-feats.sh $spurtfile $spurtdir $indir 

exit 0

