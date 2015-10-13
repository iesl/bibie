#!/bin/bash

mem="16G"

dataDir="/iesl/canvas/ksilvers/bibie-exec/data-grobid-24-Sept"

fac="factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
jarfile="bibie-0.1-SNAPSHOT.jar"

cp="$fac:$jarfile"

java -Xmx$mem -cp $cp edu.umass.cs.iesl.bibie.model.CitationTaggerTrainer \
--tagger-type="combined" \
--optimizer="lbfgs" \
--save-model="true" \
--model-file="bibie.combined.factorie" \
--train-file="$dataDir/citation4654903728793743412.train" \
--dev-file="$dataDir/citation4679257556954193486.test" \
--lexicons="file:///iesl/canvas/ksilvers/bibie-exec/src/main/resources/lexicons"


