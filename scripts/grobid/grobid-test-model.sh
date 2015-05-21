#!/bin/bash

echo $BIBROOT

memory="10G"

timestamp="36162105" # minute hour day month
optimAlg="adagrad"
trainFile="$BIBROOT/testfile.data" # on blake: /iesl/canvas/ksilvers/bibie-exec/testfile.data
outputFile="$BIBROOT/stuff.tmp" # rm this afterwards
writeEvals="true"

CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"

java -Xmx${memory} -cp $CP edu.umass.cs.iesl.bibie.TestCitationModelGrobid \
--root-dir=$BIBROOT \
--train-file=$trainFile \
--train-portion="0.8" \
--output=$outputFile \
--output-dir="$BIBROOT/results_${timestamp}_${optimAlg}_test" \
--write-evals=$writeEvals \
--model-file="$BIBROOT/citationCRF_${timestamp}_${optimAlg}.factorie" \
--optimizer=$optimAlg