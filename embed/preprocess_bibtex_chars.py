import os
import cPickle
import argparse
from collections import defaultdict
import bibtexparser
from bibtexparser.bparser import BibTexParser
from bibtexparser.customization import convert_to_unicode


def build_labels_and_vocab(args):
    label_count = defaultdict(int)
    char_count = defaultdict(int)
    dirname = args.indir
    outdir = args.outdir
    fnames = ['%s/%s' % (dirname, f) for f in os.listdir(dirname)]
    label_df = {fname: set([]) for fname in fnames}
    n = len(fnames)
    errors = []
    parser = BibTexParser()
    parser.customization = convert_to_unicode
    for i, f in enumerate(fnames):
        with open(f, 'r') as bib_file:
            bib_db = None
            try:
                bib_db = bibtexparser.load(bib_file, parser=parser)
            except Exception:
                errors.append(f.split('/')[-1])
                continue
            for entry in bib_db.entries:
                for label, contents in entry.items():
                    label_count[label] += 1
                    label_df[f].add(label)
                    for char in list(contents):
                        char_count[char] += 1
        if i % 50 == 0:
            print 'done with %d/%d' % (i, n)

    print '%d / %d files had errors' % (len(errors), n)
    errors = set(errors)
    cPickle.dump(errors, open('%s/errors.pkl' % outdir, 'w'))

    # prune labels that seldom occur
    filter_below = args.filter_below
    cutoff = int(round(filter_below * n))  # each label must occur in at least this many docs
    final_labels = {}
    for label, count in label_count.items():
        df = 0
        include = False
        for doc, label_set in label_df.items():
            if label in label_set:
                df += 1
            if df >= cutoff:
                include = True
                break
        if include:
            final_labels[label] = count
    cPickle.dump(label_count, open('%s/raw_label_counts.pkl' % outdir, 'w'))
    cPickle.dump(final_labels, open('%s/final_label_counts.pkl' % outdir, 'w'))
    label_list = sorted(list(final_labels.keys()))
    labels = {label: label_list.index(label) for label in label_list}
    cPickle.dump(labels, open('%s/labels.pkl' % outdir, 'w'))

    # prune chars that only occur once
    final_vocab = set([])
    for char, count in char_count.items():
        if count > 1:
            final_vocab.add(char)
    cPickle.dump(char_count, open('%s/raw_char_counts.pkl' % outdir, 'w'))
    vocab_list = sorted(list(final_vocab))
    vocab = {char: vocab_list.index(char) for char in vocab_list}
    cPickle.dump(vocab, open('%s/vocab.pkl' % outdir, 'w'))


def process_files(args):
    dirname = args.indir
    outdir = args.outdir
    skiplist = cPickle.load(open('%s/errors.pkl' % outdir, 'r'))
    fnames = ['%s/%s' % (dirname, f) for f in os.listdir(dirname) if f not in skiplist]
    n = len(fnames)
    vocab = cPickle.load(open('%s/vocab.pkl' % outdir, 'r'))
    labels = cPickle.load(open('%s/labels.pkl' % outdir, 'r'))
    parser = BibTexParser()
    parser.customization = convert_to_unicode
    for i, f in enumerate(fnames):
        outfile = '%s/%s' % (outdir, f.split('/')[-1])
        lines = []
        with open(f, 'r') as bib_file:
            bib_db = None
            try:
                bib_db = bibtexparser.load(bib_file, parser=parser)
            except Exception:
                continue
            for entry in bib_db.entries:
                for label, contents in entry.items():
                    if label not in labels:
                        continue
                    label_int = labels[label]
                    char_ints = []
                    for char in list(contents):
                        if char not in vocab:
                            continue
                        char_ints.append(vocab[char])
                    if len(char_ints) > 0:
                        line = '%d\t%s' % (label_int, ' '.join([str(i) for i in char_ints]))
                        lines.append(line)
        if len(lines) > 0:
            with open(outfile, 'w') as outf:
                for line in lines:
                    outf.write(line + '\n')
        if i % 50 == 0:
            print 'processed %s' % f
            print 'done with %d/%d' % (i, n)


if __name__ == '__main__':
    p = argparse.ArgumentParser(description='preprocess bibtex files for character LSTM training')
    p.add_argument('--init', required=True, type=int, help='initialize labels/vocab')
    p.add_argument('--indir', required=True, type=str)
    p.add_argument('--outdir', required=True, type=str)
    p.add_argument('--filter-below', type=float, help='filter labels that occur in fewer than this percent of bibs')
    args = p.parse_args()
    print args
    if args.init:
        build_labels_and_vocab(args)
    else:
        process_files(args)


