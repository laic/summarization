#!/bin/bash

export CLASSPATH=/home/jdriesen/work/punctuation/for_skynews_demo/mxpost/mxpost.jar

if [ $# != 1 ]
	then
	echo  "Usage: $0 <projectdir>"
	echo  "\n\nAn example for a project dir is"
	echo  "\n/group/project/nlp-speech/src/mxpost/tagger.project"
	exit 1
fi

java -mx1024m tagger.TestTagger $1
