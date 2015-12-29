# -*- coding: utf-8 -*-
from lstm_chars import pad_mask, build_model, read_model_data, write_model_data, iterate_minibatches
from hparams import HParams

import os
# from memory_profiler import profile
import cPickle
import time
import argparse

import theano
import theano.tensor as T
import numpy as np
import lasagne
import lasagne.layers as layer

SEED = 1234

def train_model(hyparams,
                train_file,
                val_file,
                test_file,
                vocab_file,
                label_file,
                log_dir,
                model_file=None,
                embeddings_file=None):
    RNG = np.random.RandomState(SEED)
    logfile = '%s/train_%s.log' % (log_dir, time.strftime('%m%d%Y-%H%M%S'))
    print 'logfile: ', logfile
    log = open(logfile, 'w+')
    vmap = cPickle.load(open(vocab_file, 'r'))
    classmap = cPickle.load(open(label_file, 'r'))
    nclasses = len(classmap)
    del classmap
    pad_char = u'♥'
    vmap[pad_char] = 0
    V = len(vmap)

    y_val, X_val = load_dataset(val_file)
    y_test, X_test = load_dataset(test_file)

    cmd = 'cat %s | wc -l > tmp' % train_file
    print cmd
    os.system(cmd)
    ntrain = int(open('tmp', 'r').read().rstrip('\n'))
    cmd = 'rm -v tmp'
    os.system(cmd)

    log.write('train size: %d\n' % ntrain)
    log.write('val size: %d\n' % len(y_val))
    log.write('test size: %d\n' % len(y_test))
    log.write('vocab size: %d\n' % V)
    log.write('nclasses: %d\n' % nclasses)
    log.write(str(hyparams) + '\n')
    log.flush()
    # Initialize theano variables for input and output
    X = T.imatrix('X')
    M = T.matrix('M')
    y = T.ivector('y')
    print 'building model'
    network = build_model(hyparams, vmap, log, nclasses, invar=X, maskvar=M)
    if model_file is not None:
        print 'loading model from %s' % model_file
        read_model_data(network, model_file)
        log.write('loaded params from file: %s\n' % model_file)
        log.flush()
    # set up training/testing functions
    output = layer.get_output(network['softmax'])
    cost = lasagne.objectives.categorical_crossentropy(output, y).mean()
    params = layer.get_all_params(network.values())
    grad_updates = None
    optim = hyparams.optimizer
    if optim == 'adagrad':
        grad_updates = lasagne.updates.adagrad(cost, params, learning_rate=hyparams.learning_rate)
    elif optim == 'adadelta':
        grad_updates = lasagne.updates.adadelta(cost, params, learning_rate=hyparams.learning_rate)
    elif optim == 'adam':
        grad_updates = lasagne.updates.adam(cost, params)
    else:
        raise Exception('unsupported optimizer: %s' % optim)
    # need to switch off droput while testing
    test_output = lasagne.layers.get_output(network['softmax'], deterministic=True)
    val_cost_fn = lasagne.objectives.categorical_crossentropy(test_output, y).mean()
    preds = T.argmax(test_output, axis=1)
    val_acc_fn = T.mean(T.eq(preds, y), dtype=theano.config.floatX)
    print 'compiling functions'
    train_fxn = theano.function([X, M, y], cost, updates=grad_updates, allow_input_downcast=True)
    val_fxn = theano.function([X, M, y], [val_cost_fn, val_acc_fn, preds], allow_input_downcast=True)

    def compute_val_error(log_file=log, X_val=X_val, y_val=y_val, prefix=''):
        print 'starting validation'
        print 'nval:', len(y_val)
        start = time.time()
        val_loss = 0.
        val_acc = 0.
        val_batches = 0
        for batch in iterate_minibatches(X_val, y_val, batchsize, shuffle=False):
            assert batch is not None
            x_val_mini, y_val_mini = batch
            v_loss, v_acc, _ = val_fxn(x_val_mini[:, :, 0], x_val_mini[:, :, 1], y_val_mini)
            val_loss += v_loss
            val_acc += v_acc
            val_batches += 1
        try:
            val_loss /= val_batches
            val_acc /= val_batches
            log_file.write('%s\tvalidation loss: %.6f\n' % (prefix, val_loss))
            log_file.write('%s\tvalidation acc: %.3f\n' % (prefix, val_acc * 100.))
#            log_file.write("%validation loss:\t\t{:.6f}\n".format(val_loss))
#            log_file.write("\t\tvalidation accuracy:\t\t{:.2f} %\n".format(val_acc * 100.))
            log_file.flush()
        except ZeroDivisionError:
            print('WARNING: val_batches == 0')
        print 'validation took %.3f s' % (time.time() - start)
        return val_loss, val_acc

    batchsize = hyparams.batchsize
    nepochs = hyparams.nepochs
    k = hyparams.kfolds
    fold_size = int(round(1. * ntrain / k))
    nbatches = int(round(1. * fold_size / batchsize))
    # valfreq = max(int(round(nbatches / 4.)), 2)
    print 'validating every %d batches' % (int(batchsize/2.))
    # log.write('validating every %d batches\n' % valfreq)
    log.flush()
    best_val_acc = -np.inf
    start = time.time()
    for epoch in xrange(nepochs):
        train_err = 0.
        train_batches = 0
        epoch_start = time.time()
        fold_count = 0
        for y_train, X_train in iterate_folds(train_file, ntrain, k):
            fold_start = time.time()
            print '[epoch %d fold %d/%d (%d examples)]' % (epoch, fold_count, k, len(y_train))
            fold_count += 1
            for batch in iterate_minibatches(X_train, y_train, batchsize, rng=RNG, shuffle=True):
                x_mini, y_mini = batch
                # print x_train.shape, y_train.shape
                train_err += train_fxn(x_mini[:, :, 0], x_mini[:, :, 1], y_mini)
                train_batches += 1
                prefix = '[epoch %d, fold %d, batch %d]' % (epoch, fold_count, train_batches)
                # print prefix, nbatches
                if (train_batches % (int(batchsize/5.))) == 0:
                   print prefix, nbatches
                if train_batches % 512 == 0:
                    print '%s validation' % prefix
                    log.write('%s\n' % prefix) 
                    val_loss, val_acc = compute_val_error(log_file=log, X_val=X_val, y_val=y_val, prefix=prefix)
                    if val_acc >= best_val_acc:
                        best_val_acc = val_acc
                        print 'best val accuracy: %.4f' % best_val_acc
                        write_model_data(network, '%s/best_lstm_model' % log_dir)
                    log.write('%s current best val accuracy: %.4f\n' % (prefix, best_val_acc * 100.))
                    log.flush()
            progress = '\tepoch %d, fold %d took %.3f s\n' % (epoch, fold_count, time.time() - fold_start)
            print progress
            log.write(progress)
            prefix = '[epoch %d, fold %d]' % (epoch, fold_count)
            log.write('%s\ttraining loss: %.6f' % (prefix, train_err/train_batches))
            log.flush()
            val_loss, val_acc = compute_val_error(log_file=log, X_val=X_val, y_val=y_val)
            if val_acc >= best_val_acc:
                best_val_acc = val_acc
                write_model_data(network, '%s/best_lstm_model' % log_dir)
            log.write('%s\tcurrent best val accuracy: %.3f\n' % (prefix, best_val_acc * 100.))
            log.flush()
            test_loss, test_acc, _ = val_fxn(X_test[:, :, 0], X_test[:, :, 1], y_test)
            log.write('%s\ttest accuracy: %.3f\n' % (prefix, test_acc * 100.))
            log.flush()
        progress = 'epoch %d took %.3f s\n' % (epoch, time.time() - epoch_start)
        print progress
        log.write(progress)
        log.write('training loss: %.6f' % (train_err/train_batches))
        log.flush()
        val_loss, val_acc = compute_val_error(log_file=log, X_val=X_val, y_val=y_val)
        if val_acc >= best_val_acc:
            best_val_acc = val_acc
            write_model_data(network, '%s/best_lstm_model' % log_dir)
        log.write('best validation accuracy: %.4f\n' % (best_val_acc * 100.))
        log.flush()
        test_loss, test_acc, _ = val_fxn(X_test[:, :, 0], X_test[:, :, 1], y_test)
        log.write('test accuracy: %.4f\n' % (test_acc * 100.))
        log.flush()
    progress = 'training took %3f s\n' % (time.time() - start)
    print progress
    log.write(progress)
    read_model_data(network, '%s/best_lstm_model' % log_dir)
    test_loss, test_acc, _ = val_fxn(X_test[:, :, 0], X_test[:, :, 1], y_test)
    log.write('final test accuracy: %.4f\n' % (test_acc * 100.))
    log.flush()
    log.close()
    return network



def process_fold(lines):
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


def iterate_folds(filename, n, k):
    fold_size = int(round(1. * n / k))
    print 'fold size: %d' % fold_size
    fin = open(filename, 'r')
    while True:
        i = 0
        fold = []
        while i < fold_size:
            line = fin.readline()
            i += 1
            if not line:
                break
            fold.append(line)
        yield process_fold(fold)


def load_dataset(filename):
    lines = open(filename, 'r').readlines()
    return process_fold(lines)


if __name__ == '__main__':
    p = argparse.ArgumentParser(description='train word-level lstm')
    # input
    p.add_argument('--tweet-file', required=True, help='path to train data')
    p.add_argument('--test-file', help='path to test file')
    p.add_argument('--dev-file', type=str, help='path to dev set')
    p.add_argument('--vocab', required=True, help='path to vocabulary')
    p.add_argument('--model-file', type=str)
    p.add_argument('--label-file', type=str)

    # output
    p.add_argument('--log-path', type=str, required=True, help='path to store log file')
    p.add_argument('--results-file', type=str, help='filename for results file')

    # hyperparameters
    p.add_argument('--nepochs', type=int, default=30, help='# of epochs')
    p.add_argument('--batchsize', type=int, default=512, help='batch size')
    p.add_argument('--kfolds', type=int, default=10, help='break train data into [kfolds] chunks')
    p.add_argument('--learning-rate', type=float, default=0.1, help='learning rate')
    p.add_argument('--bidirectional', type=int, default=1, help='bidirectional LSTM?')
    p.add_argument('--nhidden', type=int, default=256, help='num hidden units')
    p.add_argument('--embedding-dim', type=int, default=50, help='embedding size')
    p.add_argument('--pool', type=str, default='mean', help='pooling strategy')
    p.add_argument('--grad-clip', type=int, default=100, help='gradient clipping')
    p.add_argument('--optimizer', type=str, default='adam', help='optimizer')
    p.add_argument('--init', type=str, default='random', help='initialization')

    # switches
    p.add_argument('--test-only', type=int, default=0, help='just test model contained in model file')
    p.add_argument('--write-embeddings', type=int, default=0, help='write char embeddings to text format')

    args = p.parse_args()
    print("ARGS:")
    print(args)
    hyparams = HParams()
    hyparams.parse_args(args)
    print hyparams

    train_model(hyparams,
                args.tweet_file,
                args.dev_file,
                args.test_file,
                args.vocab,
                args.label_file,
                args.log_path,
                model_file=args.model_file)



# @profile
# def testit():
#     filename = '/Users/kate/research/AI2/bibie/all.train'
#     cmd = 'cat %s | wc -l > tmp' % filename
#     print cmd
#     os.system(cmd)
#     n = int(open('tmp', 'r').read().rstrip('\n'))
#     print n
#     cmd = 'rm -v tmp'
#     os.system(cmd)
#     k = 20
#     count = 0
#     for x, y in iterate_folds(filename, n, k):
#         print count, len(y)
#         count += 1
#
#
# if __name__ == '__main__':
#     testit()

