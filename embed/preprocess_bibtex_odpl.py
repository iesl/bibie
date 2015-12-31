import os
import cPickle
import argparse
from collections import defaultdict
import bibtexparser
from bibtexparser.bparser import BibTexParser
from bibtexparser.customization import convert_to_unicode
import string
import re


IGNORE_SET = set(['ENTRYTYPE',  #  e.g. 'article', 'inproceedings'
                  'ID'  # e.g. 'Kate2013', 'ratinov2009design'
                  ])


def clean_label(label):
    label = re.sub(r'%\s*', '', label)
    return label

def process_file(filename, labels):
    with open(filename, 'r') as bibfile:
        bibstr = bibfile.read()
    db = bibtexparser.loads(bibstr)
    lines = []
    for entry in db.entries:
        for label, contents in entry.items():
            label = clean_label(label)
            if label in IGNORE_SET:
                continue
            if label not in labels:
                continue
            lines.append((label, contents))
    return lines


def process_files(args):
    labels = cPickle.load(open(args.labels, 'r'))
    indir = args.indir
    fnames = ['%s/%s' % (indir, f) for f in os.listdir(indir)]
    lines = []
    for f in fnames:
        lines.extend(process_file(f, labels))
        break
    for label, contents in lines:
        print label, contents


if __name__ == '__main__':
    p = argparse.ArgumentParser(description='preprocess bibtex files for document classifier')
    p.add_argument('--init', required=False, type=int, help='initialize labels/vocab')
    p.add_argument('--indir', required=True, type=str)
    p.add_argument('--outdir', required=True, type=str)
    p.add_argument('--filter-below', type=float, help='filter labels that occur in fewer than this percent of bibs')
    p.add_argument('--labels', type=str, required=True)
    args = p.parse_args()
    process_files(args)
    
