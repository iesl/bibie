#!/bin/bash

memSize="40G"
CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"
f1=$1 # path to evaluation file 1 (e.g. on blake, /iesl/canvas/ksilvers/bibie-exec/comparison/grobid.results.token)
f2=$2 # path to evaluation file 2 (e.g. on blake, /iesl/canvas/ksilvers/bibie-exec/comparsion/00172105.results.token)

java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.bibie.Comparison $f1 $f2