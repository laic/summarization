#!/bin/bash

export CLASSPATH=$HOME/summarization/sentence_breaks/mxpost/mxpost.jar:$CLASSPATH

if [ $# != 1 ]
	then
	echo  "Usage: $0 <projectdir>"
	echo  "\n\nAn example for a project dir is"
	echo  "\n/group/project/nlp-speech/src/mxpost/tagger.project"
	exit 1
fi

java -Xmx1g tagger.TestTagger $1
