#!/bin/bash

if [ -z "$BIBROOT" ]; then
    echo "set BIBROOT environment variable"
else
    # "knobs"
    optimizer="adagrad"
    saveModel="true"
    modelFile="$BIBROOT/citationCRF.factorie"

    # data
    trainfile="$BIBROOT/umass-citation/training.docs"
    testFile="$BIBROOT/umass-citation/dev.docs"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"
    CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"

    java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.bibie.OptimizeCitationModel \
    --root-dir=$BIBROOT \
    --optimizer=$optimizer \
    --save-model=$saveModel \
    --model-file=$modelFile \
    --train-file=$trainfile \
    --test-file=$testFile \
    --lexicons=$lexicons
fi
