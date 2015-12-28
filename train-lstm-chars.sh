#!/usr/bin/env bash

root="$PWD"
dataRoot="$root/my_preproc"

infile="$dataRoot/all.train"
devfile="$dataRoot/all.dev"
testfile="$dataRoot/all.test"

vocab="$dataRoot/vocab.pkl"
labels="$dataRoot/labels.pkl"

logdir="$root/debug"
mkdir -pv $logdir

THEANO_FLAGS=device=cpu,floatX=float32 python $root/embed/lstm_chars.py \
--tweet-file $infile \
--vocab $vocab \
--log-path $logdir \
--test-file $testfile \
--dev-file $devfile \
--batchsize "100" \
--nepochs "1" \
--label-file $labels

