#!/bin/bash

memSize="2G"

grobidFile=$1
ieslFile=$2

$BIBROOT/scripts/run_class.sh -Xmx${memSize} edu.umass.cs.iesl.bibie.EvalGrobid \
--model="file://$BIBROOT/citationCRF.factorie" \
--grobid-file=$grobidFile \
--iesl-file=$ieslFile
