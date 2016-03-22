#!/bin/bash

root="$PWD"
echo $root
cd $root/data
wget https://s3.amazonaws.com/iesl-citation-data/grobid-citation.tgz
tar xzvf grobid-citation.tgz
cd -
cd $root/trained-model
wget https://s3.amazonaws.com/iesl-citation-models/CitationTagger.tgz
tar xzvf CitationTagger.tgz
echo "done"
