#!/bin/bash

iters="2"
rate="1.0"

root="/home/kate/AI2/bibie"

modelFile="$root/bibie.factorie.grobid"

#trainFile="$root/umass-citation/training.docs"
#devFile="$root/umass-citation/dev.docs"
#testFile="$root/umass-citation/testing.docs"

trainFile="/home/kate/AI2/bibie/grobid/citation8947890140946357789.train"
devFile=""
testFile="/home/kate/AI2/bibie/grobid/citation5106663471313568163.test"

mem="16G"

fac="/home/kate/research/factorie/target/factorie_2.11-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar"
jarfile="target/bibie-0.1-SNAPSHOT.jar"

cp="$fac:$jarfile"

java -Xmx$mem -cp $cp edu.umass.cs.iesl.bibie.Cli \
--optimizer="lbfgs" \
--save-model="true" \
--root-path=$root \
--model-file=$modelFile \
--train-file=$trainFile \
--dev-file=$devFile \
--test-file=$testFile \
--lexicons="file:///home/kate/AI2/bibie/src/main/resources/lexicons" \
--adagrad-rate=$rate \
--num-iterations=$iters

