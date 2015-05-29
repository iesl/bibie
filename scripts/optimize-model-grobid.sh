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
    trainPortion="0.8"

    # data
    dataSet="grobid"
    useGrobidFeatures="false"
    trainfile="$BIBROOT/grobid/trainfile.data"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"
    CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"

    java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.bibie.OptimizeCitationModel \
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
    --lexicons=$lexicons
fi