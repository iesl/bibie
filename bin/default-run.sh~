#!/bin/bash

mem="16G"

dataDir="/iesl/canvas/ksilvers/data/umass-citation"

fac="factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
jarfile="bibie-0.1-SNAPSHOT.jar"

cp="$fac:$jarfile"

java -Xmx$mem -cp $cp edu.umass.cs.iesl.bibie.model.CitationTaggerTrainer \
--tagger-type="default" \
--optimizer="adagrad" \
--save-model="true" \
--model-file="bibie.default.factorie" \
--train-file="$dataDir/training.docs" \
--dev-file="$dataDir/dev.docs" \
--test-file="$dataDir/testing.docs" \
--lexicons="file:///iesl/canvas/ksilvers/bibie-exec/src/main/resources/lexicons"
