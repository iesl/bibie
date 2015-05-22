#!/bin/bash

if [ -z "$BIBROOT" ]; then
    echo "set BIBROOT environment variable"
else
    # "knobs"
    modelFile="file://$BIBROOT/citationCRF.factorie"

    # data
    testFile="$BIBROOT/umass-citation/testing.docs"
    lexicons="file://$BIBROOT/src/main/resources/lexicons"

    memSize="10G"
    CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"

    java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.bibie.TestCitationModel \
    --root-dir=$BIBROOT \
    --model-file=$modelFile \
    --test-file=$testFile \
    --lexicons=$lexicons
fi
