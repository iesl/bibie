#!/usr/bin/env bash

root=$PWD
indir=$root/my_preproc/bibs
outdir=$root/my_preproc

python $root/embed/split_data.py \
--indir $indir \
--outdir $outdir
