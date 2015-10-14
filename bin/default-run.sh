#!/bin/bash

mem="16G"

dataDir="/iesl/canvas/ksilvers/data/umass-citation"
rootDir="/iesl/canvas/ksilvers/bibie-exec"

fac="factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
jarfile="bibie-1.0-SNAPSHOT.jar"

cp="$fac:$jarfile"

java -Xmx$mem -cp $cp edu.umass.cs.iesl.bibie.util.RunCitationTagger \
--tagger-type="default" \
--model-file="$rootDir/bibie.default.factorie" \
--test-file="$dataDir/testing.docs" \
--lexicons="file://$rootDir/src/main/resources/lexicons"
