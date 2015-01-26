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
TABLEDIR=/disk/data1/clai/work/inevent/data/tables
idfconvfile="$TABLEDIR/all.nxtwords.idf.conv.txt"
idfspkfile="$TABLEDIR/all.nxtwords.idf.spk.txt"
pmifile="$TABLEDIR/ami.icsi.eda.pmi.txt"

while getopts "c:s:p:" OPTION
do
     case $OPTION in
         c)
             idfconvfile=$OPTARG
             ;;
         s)
             idfspkfile=$OPTARG
             ;;
#         f)
#             allidfconvsfile=$OPTARG
#             ;;
         p)
             pmifile=$OPTARG
             ;;
         *)
                echo "invalid arg" 
                exit
     esac
done

shift $(($OPTIND -1))

conv=$1
corpus=$2
datadir=$3

Rscript ../rscripts/get-tf-feats.r $conv $corpus $idfconvfile $idfspkfile $pmifile $datadir 


