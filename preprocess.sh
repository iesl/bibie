#!/usr/bin/env bash

root=$PWD
indir=$root/bibtex/bibs
outdir=$root/my_preproc
mkdir -pv $outdir/bibs

labels=$outdir/labels.pkl
vocab=$outdir/vocab.pkl

python $root/embed/preprocess_bibtex_chars.py \
--indir $indir \
--outdir $outdir \
--vocab $vocab \
--labels $labels

