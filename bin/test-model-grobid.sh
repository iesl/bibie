#!/bin/bash

if [ -z "$BIBROOT" ]; then
    echo "set BIBROOT environment variable"
else
    # "knobs"
    modelFile="$BIBROOT/grobidCitationCRF.factorie"
    writeEvals="true"
    outputFile="$BIBROOT/stuff.tmp"
    outputDir="$BIBROOT/results"

    # data
    dataSet="grobid"
    useGrobidFeatures="true"
    testfile="$BIBROOT/grobid-citation-test.data"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"

    $BIBROOT/bin/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.bibie.TestCitationModel \
    --root-dir=$BIBROOT \
    --model-file=$modelFile \
    --write-evals=$writeEvals \
    --output=$outputFile \
    --output-dir=$outputDir \
    --dataset=$dataSet \
    --use-grobid-features=$useGrobidFeatures \
    --test-file=$testfile \
    --lexicons=$lexicons
fi
