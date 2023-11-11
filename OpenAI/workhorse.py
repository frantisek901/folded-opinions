#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jun  9 17:10:20 2020

@author: guga
"""
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt
from multiprocessing import Pool
from tqdm import tqdm
from common import Saveable
import random


class WorkHorse(Saveable):
    # if we should shuffle the parameters and iterations before launching
    SHUFFLE_PARAMS = True

    def __init__(self, f=None, params=None, iters=1, threads=1,
                 job=None, num_jobs=1, min_job=0):
        '''
        WorkHorse will run function for you on provided parameters
        iter-times for each value of param, calculate mean and std for each
        either sequentially (in which case it prints a progress bar)
        or in parallel
        '''
        self.job = job  # job id
        self.num_jobs = num_jobs  # total number of jobs
        self.min_job = min_job  # job id's starting index

        self.f = f  # function to execut for obtaining data
        self.pp = params  # parameters to execute f on
        if num_jobs > 1:
            self.split_params_between_jobs()  # compute subset of params for this job

        self.iters = iters  # number of runs per parameter
        self.verbose = threads == 1
        self.parallel = threads > 1  # parallelize the computation within each job
        self.threads = threads
        self.data = None
        self.raw = None
        self.f_out_num = 1  # number of outputs f returns

        self.name = ""
        self.xlabel = ""
        self.ylabels = None

        self.ignor_left = 0  # ignor this many first data points
        self.ignore_last = None  # ignor this many last data points

        self.lable_font_size = 7
        self.first_in_its_kind = False  # if true, print out progress reports

    def split_params_between_jobs(self):
        ''' extract the parameters specific to the given job '''
        job = self.job-self.min_job
        len_new_pp = int(np.ceil(len(self.pp)/self.num_jobs))
        pp_new = self.pp[min(len_new_pp*job, len(self.pp)):
                         min(len_new_pp*(job+1), len(self.pp))]
        self.pp = pp_new

    def compute_data(self):
        ''' does the heavy lyfting: runs f on params, iter times for each '''
        assert len(self.pp) > 0
        if self.f_out_num == 1:
            single_out = object()
        else:
            single_out = [object()]*self.f_out_num

        self.data = np.array([[single_out]*self.iters]*len(self.pp))
        if self.parallel:
            pp_resh = np.empty([len(self.pp), self.iters], dtype=object)
            for i, p in enumerate(self.pp):
                for j in range(self.iters):
                    pp_resh[i, j] = (p,
                                     j == 0 and self.first_in_its_kind,  # save
                                     i == 0 and j == 0  # verbose
                                     )
            pp_resh = pp_resh.reshape((self.iters*len(self.pp)))
            pool = Pool(min(self.iters*len(self.pp), self.threads))
            shuffled_pp_resh = list(pp_resh)  # Create a copy of the original list
            if self.SHUFFLE_PARAMS:
                # shuffle the pp_resh to randomize inputs and distribute the harder
                # and easier computations uniformly at random among workers
                random.shuffle(shuffled_pp_resh)
            # make the computation
            self.data = pool.starmap(self.f, shuffled_pp_resh)
            if self.SHUFFLE_PARAMS:
                # Permute the self.data back to the original order
                self.data = [
                    self.data[shuffled_pp_resh.index(elem)] for elem in pp_resh]

            pool.close()
            pool.join()
            if self.f_out_num == 1:
                self.data = np.array(self.data).reshape((len(self.pp),
                                                         self.iters))
            else:
                self.data = np.array(self.data).reshape((len(self.pp),
                                                         self.iters,
                                                         self.f_out_num))

        else:
            pbar = tqdm(total=len(self.pp)*self.iters)
            for (i, p) in enumerate(self.pp):
                for j in range(self.iters):
                    self.data[i, j] = self.f(p)
                    pbar.update(1)
            pbar.close()

    def compute_means_and_errors(self):
        ''' from data it computes means and errors for each param value '''
        self.means = np.array(
            [np.mean(self.data[i], axis=0)
             for i in range(len(self.pp))])
        try:
            self.errors = np.array([stats.sem(self.data[i], axis=0)
                                    for i in range(len(self.pp))])
        except:
            print('cant calculate standard errors')

        self.stdevs = np.array(
            [stats.tstd(self.data[i], axis=0)
             for i in range(len(self.pp))])

    def plot_single(self, j, ax=plt, fmt='.', **kwargs):
        ''' plots the means with error bars as a function of params '''
        xx = self.pp
        if not hasattr(self, 'ignor_left'):  # obsolete
            self.ignor_left = 0
        if not hasattr(self, 'ignore_last'):  # obsolete
            self.ignore_last = None
        st = self.ignor_left
        nd = self.ignore_last
        if nd is not None:
            nd = -nd
        if self.f_out_num == 1:
            yy = self.means
            ee = self.errors
            ax.errorbar(xx[st:nd], yy[st:nd], yerr=ee[st:nd], fmt=fmt)
            if self.ylabels is not None:
                plt.ylabel(self.ylabels[0])
            if self.xlabel is not None:
                plt.xlabel(self.xlabel)
        else:
            yy = self.means[:, j]
            ee = self.errors[:, j]
            ax.errorbar(xx[st:nd], yy[st:nd], yerr=ee[st:nd],
                        fmt=fmt, label=self.ylabels[j].replace('\n', ' '))
            if self.ylabels is not None:
                if ax != plt:
                    plt.sca(ax)
                if not hasattr(self, 'lable_font_size'):  # obsolete
                    self.lable_font_size = 7
                plt.ylabel(self.ylabels[j], fontsize=self.lable_font_size)
            if self.xlabel is not None:
                if ax != plt:
                    plt.sca(ax)
                plt.xlabel(self.xlabel)

        ax.ticklabel_format(style='sci', axis='both', scilimits=(-2, 2))
        plt.show()

        return xx, yy, ee

    def plot_as_subplots(self, lst=None):
        ''' plots different outputs in different subplots '''
        if lst is None:
            lst = range(self.f_out_num)

        fig = plt.gcf()
        axax = [fig.add_subplot(len(lst), 1, 1)]
        axax += [fig.add_subplot(len(lst), 1, i+1, sharex=axax[0])
                 for i in range(1, len(lst))]
        # fig, axax = plt.subplots(len(lst), sharex=True, sharey=False, \
        #                         gridspec_kw={'hspace': 0})
        plt.xlabel(self.xlabel)
        # plt.suptitle(self.name, fontsize=10)
        for num, jj in enumerate(lst):
            if type(jj) is list:
                for j in jj:
                    self.plot_single(j, ax=axax[num])
                axax[num].legend(fontsize=self.lable_font_size)
                axax[num].set_ylabel('')
            else:
                self.plot_single(jj, ax=axax[num])

        # Hide x labels and tick labels for all but bottom plot.
        for ax in axax:
            ax.label_outer()

        self.axax = axax

    def plot(self, lst=None, **kwargs):
        ''' plots all f outputs as a single graph '''
        plt.suptitle(self.name, fontsize=10, wrap=True)
        if lst is None:
            lst = range(self.f_out_num)
        for j in lst:
            self.plot_single(j, **kwargs)
        if len(lst) > 1:
            plt.legend()

    def work(self):
        ''' run the f; compute means and errors; plot results '''
        self.compute_data()
        self.compute_means_and_errors()
        if self.f_out_num == 1:
            self.plot()
        else:
            self.plot_as_subplots()

    @staticmethod
    def merge(whs, verbose=False):
        ''' merge workhorses together and return '''
        data = {}
        pp = {}

        mergers = whs
        if verbose:
            mergers = tqdm(whs)
        for wh in mergers:
            for (i, p) in enumerate(wh.pp):
                try:
                    dt = np.ndarray.tolist(wh.data[i])
                except:
                    dt = wh.data[i]
                if str(p) not in pp:
                    pp[str(p)] = p
                    data[str(p)] = dt
                else:
                    data[str(p)].extend(dt)

        merged = WorkHorse()
        merged.f_out_num = whs[0].f_out_num
        merged.pp = []
        merged.data = []

        for pstr in pp:
            merged.pp.append(pp[pstr])
            merged.data.append(data[pstr])
        # np.array(sorted(pp.values()))
        # np.array([data[str(p)] for p in merged.pp])

        merged.name = whs[0].name
        merged.xlabel = whs[0].xlabel
        merged.ylabels = whs[0].ylabels

        merged.compute_means_and_errors()

        return merged

    @staticmethod
    def assamble(n, base):
        ''' read in wh files, merge and save '''
        whs = [0]*n

        print("loading and trimming")
        for i in tqdm(range(n)):
            whs[i] = WorkHorse.load(base+f' job {i}')

        print("merging")
        wh = WorkHorse.merge(whs, verbose=True)

        print("saving...")
        wh.compute_means_and_errors()
        wh.save(base)

        print("done")

    def __str__(self):
        return "WH " + self.name + ('' if self.job is None
                                    else f' job {self.job}')
