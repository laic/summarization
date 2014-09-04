#!/bin/bash

#$ -P inf_hcrc_cstr_inevent
#$ -cwd
#$ -o ./edout
#$ -e ./edout
#$ -l h_rt=06:00:00
# Initialise environment module

. /etc/profile.d/modules.sh

export PATH=~/local/bin:$PATH
export DATADIR=$DATADIR

# Run the program

idfconvfile="~/data/inevent/tables/all.nxtwords.idf.conv.txt"
idfspkfile="~/data/inevent/tables/all.nxtwords.idf.spk.txt"
pmifile="~/data/inevent/tables/ami.icsi.eda.pmi.txt"

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

Rscript ../rscripts/get-tf-feats.r $conv $corpus $idfconvfile $idfspkfile $pmifile


