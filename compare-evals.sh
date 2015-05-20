#!/bin/bash

memSize="40G"
CP="$BIBROOT/target/bibie-0.1-SNAPSHOT-jar-with-dependencies.jar"
f1="$BIBROOT/myresults/36112005.results.field"
f2="$BIBROOT/grobid/58102005.results.field"

java -Xmx${memSize} -cp $CP edu.umass.cs.iesl.bibie.Comparison $f2 $f1