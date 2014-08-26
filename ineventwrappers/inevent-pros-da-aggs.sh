#!/bin/bash
##############################################################
# Contextual info for DAs
##############################################################
echo "****************************"
conv=$1
datadir=/exports/home/clai/data/inevent/derived/segs/
hashead=F
corpus="inevent"

featnames="niteid wstart wend silence mean.normF0 median.normF0 sd.normF0 q1.normF0 q99.normF0 slope.normF0"
varname="f0-da"
featname="f0"
fsuffix=".aggs.asrutt.txt"

echo $featnames

./get-tf-seq-sub.sh -p 4 -n 4 -f $fsuffix -x $conv -d $datadir $featname $varname $corpus "$featnames"


featnames="niteid wstart wend mean.normI0 median.normI0 sd.normI0 q1.normI0 q99.normI0 slope.normI0"
varname="i0-da"
featname="i0"
fsuffix=".aggs.asrutt.txt"

./get-tf-seq-sub.sh -p 4 -n 4 -f $fsuffix -x $conv -d $datadir $featname $varname $corpus "$featnames"

echo "****************************"
