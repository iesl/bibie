#!/bin/bash

######
# Use this script as a template for training new models.
######

mem="8G"
root="$PWD"

# set these to the appropriate paths
trainFile=""
devFile=""

# where to serialize the model
modelFile="$root/CitationTagger.factorie"

# valid data types: grobid|iesl (see data/README.md for info on how to obtain the data)
dataType="grobid"

# valid tagger types: default|grobid|combined
taggerType="default"

# path to lexicons e.g. file:///iesl/canvas/ksilvers/bibie-exec/src/main/resources/lexicons
lexicons=""

### set up the classpath ###
factorieJar=""           # /path/to/factorie/target/factorie_-nlp-jar-with-dependencies.jar
bibieJar=""              #/path/to/bibie-1.0-SNAPSHOT.jar
CP="$factorieJar:$bibieJar"

java -Xmx$mem -cp $CP edu.umass.cs.iesl.paperheader.model.HeaderTaggerTrainer \
--train-file=$trainFile \
--dev-file=$devFile \
--tagger-type=$taggerType \
--data-type=$dataType \
--save-model="true" \
--model-file=$modelFile \
--optimizer="adagrad" \
--lexicons=$lexicons

