#!/bin/bash

SBDIR=../sentence_breaks/
# first prepare text corpora
echo "--- get .txt ---"

for text in $HOME/data/ted/traintrans/*sentences.txt
do
	dos2unix $text
	sed -f $SBDIR/Treebank_tokenization.sed < ${text} > ${text//.txt/_tok.txt}
	$SBDIR/my_mxpost.sh $SBDIR/mxpost/tagger.project/ < ${text//.txt/_tok.txt} > ${text//.txt/_pos.txt}
	ln -s ${text//.txt/_pos.txt}
done

cat *_pos.txt | sort -R -u |  gawk '{if($0!~"^[[:space:]]+$") {gsub("\\._\\.","|",$0); gsub("[[:punct:]]+_[[:punct:]]+","",$0); print $0}}' > randomized_train_pos_input

echo "--- columnate ---"

win_size=7
$SBDIR/columnate.py randomized_train_pos_input ${win_size} > pos_${win_size}
rm randomized_train_pos_input.gz
gzip randomized_train_pos_input

# # this is way too big for processing, cut down to 200e6 words (train and dev together)
# mv cat pos_${win_size} too_big_pos_${win_size} && cat too_big_pos_${win_size} | head -n 200000000 > pos_${win_size} && gzip too_big_pos_${win_size}

echo "--- columnate ---"
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

