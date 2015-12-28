import os
import cPickle
import argparse
import bibtexparser as bparser

class Entry(object):
    def __init__(self, original, lines):
        self.original = original
        self.lines = lines

LABEL_COUNTS = {}
LABEL_DICT = {}
CHAR_VOCAB = set([])
CHAR_DICT = {}

def parse_file(filename):
    with open(filename, 'r') as bfile:
        bstr = bfile.read()
    bdb = bparser.loads(bstr)
    return bdb.entries

def build_labeldict_and_vocab(entries):
    for entry in entries:
        for k, v in entry.items():
            if k not in LABEL_COUNTS:
                LABEL_COUNTS[k] = 0
            LABEL_COUNTS[k] += 1
            for c in list(v):
                CHAR_VOCAB.add(c)

def finalize_labeldict_and_vocab():
    chars = list(CHAR_VOCAB)
    for i, c in enumerate(chars):
        CHAR_DICT[c] = i

    cPickle.dump(LABEL_COUNTS, open('label_counts.pkl', 'w'))
    #  prune out labels that occur only once
    labels_pruned = {}
    for k, v in LABEL_COUNTS.items():
        if v > 1:
            labels_pruned[k] = v
    labels = list(labels_pruned.keys())
    for i, l in enumerate(labels):
        LABEL_DICT[l] = i


def entries2ints(entries):
    lines = []
    for entry in entries:
        for k, v in entry.items():
            if k not in LABEL_DICT:
                print 'not in label set:', k
                continue
            lint = LABEL_DICT[k]
            cints = []
            for c in list(v):
                if c in CHAR_DICT:
                    cints.append(CHAR_DICT[c])
            line = '%d\t%s' % (lint, ' '.join([str(cint) for cint in cints]))
            lines.append(line)
    return lines


def doit(args):
    global CHAR_DICT
    global LABEL_DICT
    dirname = args.indir
    outdir = args.outdir
    fnames = ['%s/%s' % (dirname, f) for f in os.listdir(dirname)]
    if args.vocab and args.labels:
        CHAR_DICT = cPickle.load(open(args.vocab, 'r'))
        LABEL_DICT = cPickle.load(open(args.labels, 'r'))
    else:
        print 'building vocab and label set'
        errors = []
        for fname in fnames:
            try:
                entries = parse_file(fname)
                build_labeldict_and_vocab(entries)
            except Exception as e:
                print e
                errors.append(fname)
                pass
        print 'errors in %d files' % (len(errors))
        err_fname = '%s/%s' % (outdir, 'errors.txt')
        with open(err_fname, 'w') as f:
            for ef in errors:
                f.write(ef + '\n')
        finalize_labeldict_and_vocab()
        print 'writing vocab'
        vocab_file = '%s/%s' % (outdir, 'vocab.pkl')
        cPickle.dump(CHAR_DICT, open(vocab_file, 'w'))
        print 'writing labels'
        label_file = '%s/%s' % (outdir, 'labels.pkl')
        cPickle.dump(LABEL_DICT, open(label_file, 'w'))
    print LABEL_DICT
    print CHAR_DICT
    count = 0
    n = len(fnames)
    print 'processing files'
    for fname in fnames:
        base_fname = fname.split('/')[-1]
        outfile = '%s/bibs/%s' % (outdir, base_fname)
        try:
            entries = parse_file(fname)
            lines = entries2ints(entries)
            with open(outfile, 'w') as outf:
                for line in lines:
                    outf.write(line + '\n')
        except Exception:
            pass
        count += 1
        if count % 10 == 0:
            print 'done with %d/%d files' % (count, n)


if __name__ == '__main__':
    p = argparse.ArgumentParser(description='preprocess bibtex files for character LSTM training')
    p.add_argument('--indir', required=True, type=str)
    p.add_argument('--outdir', required=True, type=str)
    p.add_argument('--vocab', type=str)
    p.add_argument('--labels', type=str)
    args = p.parse_args()
    print args
    doit(args)


