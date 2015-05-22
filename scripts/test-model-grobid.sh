#!/bin/bash

if [ -z "$BIBROOT" ]; then
    echo "set BIBROOT environment variable"
else
    # "knobs"
    modelFile="file://$BIBROOT/grobidCitationCRF.factorie"
    writeEvals="true"
    outputFile="$BIBROOT/stuff.tmp"
    outputDir="$BIBROOT/results"

    # data
    dataSet="grobid"
    useGrobidFeatures="false"
    testfile="$BIBROOT/grobid/testfile.data"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"
    CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"

    java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.bibie.TestCitationModel \
    --root-dir=$BIBROOT \
    --model-file=$modelFile \
    --write-evals=$writeEvals \
    --output=$outputFile \
    --output-dir=$outputDir \
    --data-set=$dataSet \
    --use-grobid-features=$useGrobidFeatures \
    --test-file=$testfile \
    --lexicons=$lexicons
fi