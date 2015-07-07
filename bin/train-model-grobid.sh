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
    trainPortion="0.1"
    testPortion="0.1"


    l1=0.39239924814684846
    l2=4.509018585549217E-6
    rate=0.09659744095190409
    delta=0.08562651740304852
    numIterations=1

    # data
    dataSet="grobid"
    useGrobidFeatures="true"
    trainfile="$BIBROOT/grobid-citation-train.data"
    testfile="$BIBROOT/grobid-citation-test.data"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"

    $BIBROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.bibie.TrainCitationModel \
    --root-dir=$BIBROOT \
    --optimizer=$optimizer \
    --save-model=$saveModel \
    --model-file=$modelFile \
    --write-evals=$writeEvals \
    --output=$outputFile \
    --output-dir=$outputDir \
    --train-portion=$trainPortion \
    --test-portion=$testPortion \
    --dataset=$dataSet \
    --use-grobid-features=$useGrobidFeatures \
    --train-file=$trainfile \
    --test-file=$testfile \
    --lexicons=$lexicons \
    --l1=$l1 \
    --l2=$l2 \
    --adagrad-rate=$rate \
    --adagrad-delta=$delta \
    --num-iterations=$numIterations
fi
