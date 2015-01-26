#!/bin/bash
##############################################################
# Contextual info for DAs
##############################################################
echo "****************************"
conv=$1
wtype=$2
segsdir=$3

fsuffix=".aggs.$wtype.txt"
hashead=F
corpus="inevent"

featnames="niteid wstart wend silence mean.normF0 median.normF0 sd.normF0 q1.normF0 q99.normF0 slope.normF0"
varname="f0-$wtype"
featname="f0"

echo $featnames

./get-tf-seq-sub.sh -p 4 -n 4 -f $fsuffix -x $conv -d $segsdir $featname $varname $corpus "$featnames"

featnames="niteid wstart wend mean.normI0 median.normI0 sd.normI0 q1.normI0 q99.normI0 slope.normI0"
varname="i0-$wtype"
featname="i0"

echo $featnames

./get-tf-seq-sub.sh -p 4 -n 4 -f $fsuffix -x $conv -d $segsdir $featname $varname $corpus "$featnames"

echo "****************************"
