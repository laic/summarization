#!/bin/bash

SBDIR=../sentence_breaks/
##########################################################################
# test
ctmdir=$HOME/data/ted/ctm/
win_size=7
cp pos_${win_size}.tree try.tree
cp pos_${win_size}.names try.names

for tctm in ${ctmdir}/*.ctm
do
	echo "tokenize"
	cat $tctm | gawk 'BEGIN{str=""; sep=""} {str=sprintf("%s%s%s",str,sep,tolower($5)); sep=" ";} END{print str}' > test.txt
	sed -f $SBDIR/Treebank_tokenization.sed < test.txt > test_tok.txt
	$SBDIR/my_mxpost.sh $SBDIR/mxpost/tagger.project/ < test_tok.txt > test_pos.txt

	echo "to columns"
	$SBDIR/columnate_test.py test_pos.txt ${win_size} words_${win_size} > try.data

	echo "to columns"
	c5.0print -f try | gawk 'BEGIN{getline;getline;getline; while(getline==1) print $0;}' > intermediate_result_${win_size}
	paste -d '\t' words_${win_size} intermediate_result_${win_size} > x && mv x intermediate_result_${win_size}

	$SBDIR/result_align.py intermediate_result_${win_size} ${tctm} >> ${ctm//.ctm/_split.ctm} 
#	rm -f try.* test_pos_${win_size} words_${win_size} intermediate_result_${win_size}
done
#rm -rf $tempdir

