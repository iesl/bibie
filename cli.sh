#!/bin/bash

mem="16G"

fac="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
jarfile="target/bibie-0.1-SNAPSHOT.jar"

cp="$fac:$jarfile"

java -Xmx$mem -cp $cp edu.umass.cs.iesl.bibie.Cli \
--optimizer="lbfgs" \
--save-model="true" \
--model-file="bibie.factorie" \
--train-file="/home/kate/AI2/bibie/umass-citation/training.docs" \
--dev-file="/home/kate/AI2/bibie/umass-citation/dev.docs" \
--test-file="/home/kate/AI2/bibie/umass-citation/testing.docs" \
--lexicons="file:///home/kate/AI2/bibie/src/main/resources/lexicons"
