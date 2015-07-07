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

    $BIBROOT/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.bibie.TrainCitationModel \
    --root-dir=$BIBROOT \
    --optimizer=$optimizer \
    --save-model=$saveModel \
    --model-file=$modelFile \
    --train-file=$trainfile \
    --test-file=$testFile \
    --lexicons=$lexicons
fi
