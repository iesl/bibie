#!/bin/bash

if [ -z "$BIBROOT" ]; then
    echo "set BIBROOT environment variable"
else
    # "knobs"
    optimizer="adagrad"
    saveModel="true"
    modelFile="$BIBROOT/grobidCitationCRF.factorie"
    writeEvals="true"
    outputFile="$BIBROOT/stuff.tmp"
    outputDir="$BIBROOT/results"
    trainPortion="1.0"

    # data
    dataSet="grobid"
    useGrobidFeatures="true"
    trainfile="$BIBROOT/grobid-citation-train.data"
    testfile="$BIBROOT/grobid-citation-test.data"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"

    $BIBROOT/scripts/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.bibie.OptimizeCitationModel \
    --root-dir=$BIBROOT \
    --optimizer=$optimizer \
    --save-model=$saveModel \
    --model-file=$modelFile \
    --write-evals=$writeEvals \
    --output=$outputFile \
    --output-dir=$outputDir \
    --train-portion=$trainPortion \
    --data-set=$dataSet \
    --use-grobid-features=$useGrobidFeatures \
    --train-file=$trainfile \
    --test-file=$testfile \
    --lexicons=$lexicons
fi
