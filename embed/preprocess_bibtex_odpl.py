import os
import cPickle
import argparse
import codecs
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
    try:
        db = bibtexparser.loads(bibstr)
    except Exception:
        return None
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

def get_filenames(indir, filelist_file):
    filelist = []
    with open(filelist_file, 'r') as f:
        for line in f:
            filelist.append(line.rstrip('\n'))
    filelist = set(filelist)
    files = ['%s/%s' % (indir, f) for f in os.listdir(indir) if f in filelist]
    return files

def process_files(filenames, outdir, setid, labels):
    lines = []
    for i, f in enumerate(filenames):
        processed = process_file(f, labels)
        if processed:
            lines.extend(processed)
        if i % 500 == 0:
            print 'processed ', i, 'files'
    nerrs = 0
    outfile = '%s/%s' % (outdir, '%s.txt' % setid)
    outf = codecs.open(outfile, 'w', errors='ignore')
    for label, contents in lines:
        try:
            outf.write('>>>NEWDOC\n')
            outf.write('%s\n' % label)
            outf.write('%s\n' % contents)
        except Exception:
            nerrs += 1
            continue
    outf.close()
    print 'errors:', nerrs

def main(args):
    labels = cPickle.load(open(args.labels, 'r'))
    print labels
    indir = args.indir
    outdir = args.outdir
    trainfiles = get_filenames(indir, args.trainlist)
    process_files(trainfiles, outdir, 'train', labels)
    devfiles = get_filenames(indir, args.devlist)
    process_files(devfiles, outdir, 'dev', labels)
    testfiles = get_filenames(indir, args.testlist)
    process_files(testfiles, outdir, 'test', labels)




if __name__ == '__main__':
    p = argparse.ArgumentParser(description='preprocess bibtex files for document classifier')
    p.add_argument('--init', required=False, type=int, help='initialize labels/vocab')
    p.add_argument('--indir', required=True, type=str)
    p.add_argument('--trainlist', type=str, required=True)
    p.add_argument('--devlist', type=str, required=True)
    p.add_argument('--testlist', type=str, required=True)
    p.add_argument('--outdir', required=True, type=str)
    p.add_argument('--filter-below', type=float, help='filter labels that occur in fewer than this percent of bibs')
    p.add_argument('--labels', type=str, required=True)
    args = p.parse_args()
    main(args)

    
