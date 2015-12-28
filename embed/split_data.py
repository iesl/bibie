import os
import argparse
import numpy as np

SEED = 1234

def do_split(indir, outdir, proportions=[0.7, 0.2, 0.1]):
    rng = np.random.RandomState(SEED)
    base_fnames = [f for f in os.listdir(indir)]
    full_fnames = ['%s/%s' % (indir, f) for f in base_fnames]
    n = len(full_fnames)
    all_idx = np.arange(n)
    rng.shuffle(all_idx)
    ntrain = int(round(proportions[0]*n))
    train_idx = all_idx[:ntrain]
    ndev = proportions[1]*n
    dev_idx = all_idx[ntrain:ntrain+ndev]
    test_idx = all_idx[ntrain+ndev:]
    print 'train:', len(train_idx), 1. * len(train_idx) / n
    print 'dev:', len(dev_idx), 1. * len(dev_idx) / n
    print 'test:', len(test_idx), 1. * len(test_idx) / n
    check = len(train_idx) + len(dev_idx) + len(test_idx)
    print 'n:', n, 'check:', check

    # train = full_fnames[train_idx]
    # dev = full_fnames[dev_idx]
    # test = full_fnames[test_idx]

    traindir = '%s/%s' % (indir, 'train')
    devdir = '%s/%s' % (indir, 'dev')
    testdir = '%s/%s' % (indir, 'test')

    for d in [traindir, devdir, testdir]:
        os.system('mkdir -pv %s' % d)

    for idx in train_idx:
        fname = full_fnames[idx]
        cmd = 'mv %s %s' % (fname, traindir)
        # print cmd
        os.system(cmd)

    for idx in dev_idx:
        fname = full_fnames[idx]
        cmd = 'mv %s %s' % (fname, devdir)
        # print cmd
        os.system(cmd)

    for idx in test_idx:
        fname = full_fnames[idx]
        cmd = 'mv %s %s' % (fname, testdir)
        # print cmd
        os.system(cmd)

    # for t in train:
    #     cmd = 'mv %s %s' % (t, traindir)
    #     print cmd
    #     break


if __name__ == '__main__':
    p = argparse.ArgumentParser(description='preprocess bibtex files for character LSTM training')
    p.add_argument('--indir', required=True, type=str)
    p.add_argument('--outdir', required=True, type=str)
    p.add_argument('--vocab', type=str)
    p.add_argument('--labels', type=str)
    args = p.parse_args()
    print args
    do_split(args.indir, args.outdir)




