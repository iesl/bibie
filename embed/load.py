import numpy as np
from lstm_chars import pad_mask
from memory_profiler import profile


def lazy_load(filename, n, k):
    '''
    Lazily load data because memory
    :param filename: Lazily load data from filename
    :param n: Total lines in file
    :param k: Number of chunks to make
    :return: Generator on file that reads one k-sized chunk at a time
    '''
    fold_size = int(round(1. * n / k))
    print 'n=%d, k=%d, foldsize=%d' % (n, k, fold_size)
    fin = open(filename, 'r')
    for i in xrange(k):
        chunk = []
        for j in xrange(fold_size):
            chunk.append(fin.readline())
        yield process(chunk)


def regular_load(filename):
    with open(filename, 'r') as f:
        return process(f.readlines())


def process(lines):
    nerrs = 0
    X, Y = [], []
    for line in lines:
        parts = line.strip().split('\t')
        if len(parts) != 2:
            nerrs += 1
            continue
        y, x = parts[0], parts[1]
        if len(x) == 0:
            nerrs += 1
            continue
        y = int(y)
        x = map(int, x.split(' '))
        x = map(lambda v: v + 1, x)
        Y.append(y)
        X.append(x)
    Y = np.asarray(Y, dtype=np.int32)
    X = pad_mask(X)
    return Y, X


@profile
def testit():
    filename = "/Users/kate/research/AI2/bibie/all.train.small"
    n = 100000
    k = 20
    for iter in xrange(5):
        count = 0
        for y, x in lazy_load(filename, n, k):
            print iter, count, len(y)
            count += 1
    print 'done'


if __name__ == '__main__':
    testit()
