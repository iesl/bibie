#!/bin/bash

if [ -z "$BIBROOT" ]; then
    echo "set BIBROOT environment variable"
else
    trainfile="umass-citation/training.docs"
    testfile="umass-citation/testing.docs"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"
    memSize="40G"
    java -Xmx${memSize} -cp target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar \
    edu.umass.cs.iesl.bibie.TrainCitationModel --train-file=$trainfile --test-file=$testfile --lexicons=$lexicons
fi