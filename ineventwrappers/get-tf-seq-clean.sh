#!/bin/bash
pwindow=$1
nwindow=$2
varid=$3
datadir=$4
fsuffix=$5
#cat $datadir/train/*[^-]$varid.txt.$pwindow.$nwindow* > $datadir/nn/$varid-$pwindow.$nwindow-train.txt
#cat $datadir/dev/*[^-]$varid.txt.$pwindow.$nwindow* > $datadir/nn/$varid-$pwindow.$nwindow-dev.txt
#cat $datadir/test/*[^-]$varid.txt.$pwindow.$nwindow* > $datadir/nn/$varid-$pwindow.$nwindow-eval.txt

cat $datadir/train/*[^-]$varid.$pwindow.$nwindow*.$fsuffix.txt > $datadir/nn/$varid-$pwindow.$nwindow.$fsuffix-train.txt
cat $datadir/dev/*[^-]$varid.$pwindow.$nwindow*.$fsuffix.txt > $datadir/nn/$varid-$pwindow.$nwindow.$fsuffix-dev.txt
cat $datadir/test/*[^-]$varid.$pwindow.$nwindow*.$fsuffix.txt > $datadir/nn/$varid-$pwindow.$nwindow.$fsuffix-eval.txt

