inEvent Summarization Code
=============

Tools for extractive summarization based on lexical and prosodic features for the inEvent Project.
Most of the code is written in R with bash shell script wrappers.

Requires: 
- praat (for prosodic features)
- R (3.1.1 or higher)
- python: Theano (for MLP)

The main wrapper is ineventwrappers/inevent-summarization.sh which at the moment takes as input:
* ASR output in JSON format (Augmented ASR in the inEvent API)
* A .wav file 
* A media file (e.g. .mp4)
* The working directory
It returns a JSON file of time aligned quotes from with extractive summary scores (probabilities).

Note a large amount of extra feature files and debug information is currently produced.
More documentation will appear in the future.

Notes: 

* Although the relevant function is called extract-spurt-feats.sh, we 
could be using any sort of segment really with a start and endtime.  For this
system, this is output after processing ASR json files. 


