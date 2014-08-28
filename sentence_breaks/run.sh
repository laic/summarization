#!/bin/bash

# first prepare text corpora
for text in /home/jdriesen/work/euronews/euronews_en/data/meta/language_modelling/raw/*.txt
do
	dos2unix $text
	sed -f Treebank_tokenization.sed < ${text} > ${text//.txt/_tok.txt}
	./my_mxpost.sh mxpost/tagger.project/ < ${text//.txt/_tok.txt} > ${text//.txt/_pos.txt}
	ln -s ${text//.txt/_pos.txt}
done


gunzip -c *_pos.txt.gz | sort -R -u |  gawk '{if($0!~"^[[:space:]]+$") {gsub("\\._\\.","|",$0); gsub("[[:punct:]]+_[[:punct:]]+","",$0); print $0}}' > randomized_train_pos_input

win_size=7
./columnate.py randomized_train_pos_input ${win_size} > pos_${win_size}
gzip randomized_train_pos_input

# # this is way too big for processing, cut down to 200e6 words (train and dev together)
# mv cat pos_${win_size} too_big_pos_${win_size} && cat too_big_pos_${win_size} | head -n 200000000 > pos_${win_size} && gzip too_big_pos_${win_size}

nr=`cat pos_${win_size} | wc -l`
part=`echo $nr | gawk '{print int(0.9*$1)}'`
cat pos_${win_size} | head -n $part > pos_${win_size}.data
cat pos_${win_size} | tail -n `echo "$part" "$nr" | gawk '{print $1-$2}'` > pos_${win_size}.test

(
	echo "breaklabel."
	for a in `seq 1 ${win_size}`;
	do
		num=`echo "${a} ${context_size}" | gawk '{print int($1-$2-1)}'`
		echo "tag"${num}":	discrete 100"
	done
	echo "breaklabel:	CONT,STOP"
) > pos_${win_size}.names
c5.0 -b -f pos_${win_size} -o pos_${win_size}.out

# test
cp pos_${win_size}.tree try.tree
cp pos_${win_size}.names try.names

ctm=08_01_2014_00-00-00.ctm
tempdir=`mktemp -d tempXXX`
cat $ctm | gawk -v "od=${tempdir}" '{print $0 >> (od "/" $1 ".ctm")}'
rm -f ${ctm//.ctm/_split.ctm}
for tctm in ${tempdir}/*.ctm
do
	cat $tctm | gawk 'BEGIN{str=""; sep=""} {str=sprintf("%s%s%s",str,sep,tolower($5)); sep=" ";} END{print str}' > test.txt
	sed -f Treebank_tokenization.sed < test.txt > test_tok.txt
	./my_mxpost.sh mxpost/tagger.project/ < test_tok.txt > test_pos.txt

	./columnate_test.py test_pos.txt ${win_size} words_${win_size} > try.data
	c5.0print -f try | gawk 'BEGIN{getline;getline;getline; while(getline==1) print $0;}' > intermediate_result_${win_size}
	paste -d '\t' words_${win_size} intermediate_result_${win_size} > x && mv x intermediate_result_${win_size}

	./result_align.py intermediate_result_${win_size} ${tctm} >> ${ctm//.ctm/_split.ctm} 
	rm -f try.* test_pos_${win_size} words_${win_size} intermediate_result_${win_size}
done
rm -rf $tempdir

