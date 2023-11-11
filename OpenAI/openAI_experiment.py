import numpy as np
import random
import openai
import numbers
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import os
import copy
from tqdm import tqdm
import time
from timeout_decorator import timeout
from common import Saveable
from mod_analysis import data_plot

openai.api_key = open(os.path.expanduser("~")+"/.openai_api_key", "r").read()


class InfoDataBase:
    ''' knows and keeps pro/contra information, function-calling info, and request
    message contents, including system message for GPT '''

    def __init__(self, name, system_messamge, pro, contra, func, summary_request=None):
        self.name = name
        self.system_messamge = system_messamge
        self.pro = pro
        self.contra = contra
        self.func = func
        self.summary_request = summary_request

    # def sort_pros_and_cons(self, iters=10):
    #     pass

    def get_info_list(self, num_pro, num_contra, preshuffle=False, postshuffle=False,
                      pro_first=True, mix=True):
        '''get a list of pro and contra bits of information'''
        pro_local = copy.deepcopy(self.pro)
        contra_local = copy.deepcopy(self.contra)
        if preshuffle:
            random.shuffle(pro_local)
            random.shuffle(contra_local)

        pro_local = pro_local[:num_pro]
        contra_local = contra_local[:num_contra]
        if postshuffle:
            random.shuffle(pro_local)
            random.shuffle(contra_local)

        info_list = pro_local+contra_local if pro_first else contra_local+pro_local

        if mix:
            random.shuffle(info_list)
        return info_list

    def __len__(self):
        return min(len(self.pro), len(self.contra))


class GptMediator:
    ''' takes care of communication with OpenAI '''
    TIMEOUT = 20

    def __init__(self, model="gpt-3.5-turbo", temperature=0.7):
        self.model = model
        self.temp = temperature

    def retry(f):
        ''' a decorator for calling f in a loop as long as it throws an exception '''
        def wrapper(*args, **kwargs):
            while True:
                try:
                    return f(*args, **kwargs)
                except Exception as e:
                    print(f"Caught exception: {e}. Retrying...")
                    time.sleep(1)  # Add a delay between retries
        return wrapper

    @retry
    @timeout(TIMEOUT)
    def respond(self, log):
        ''' provide gpt api with the info in the log and request a text response '''
        messages = GptMediator.log_to_messages(log)
        response = openai.ChatCompletion.create(
            model=self.model,
            messages=messages,
            temperature=self.temp
        )
        return response.choices[0].message["content"]

    @retry
    @timeout(TIMEOUT)
    def function_call(self, log, func):
        ''' provide gpt api with the info in the log and request a function call '''
        messages = GptMediator.log_to_messages(log)
        response = openai.ChatCompletion.create(
            model=self.model,
            messages=messages,
            temperature=self.temp,
            functions=[func],
            function_call={"name": func["name"]}
        )
        return_var_name = func["parameters"]["required"][0]
        return eval(response.choices[0].message["function_call"]["arguments"]
                    )[return_var_name]

    @staticmethod
    def log_to_messages(log):
        ''' convert ExpSubject log to messages for the OpenAI api '''
        messages = []
        for role, text in log:
            messages.append({"role": role, "content": text})
        return messages


class ExpSubject:
    ''' simulating an experimental subject in a social study '''

    def __init__(self, summarize_all, summarize_last, full_memory, system_messamge,
                 func, gpt_mediator=None, summary_request=None, **more_params):
        self.log = [("system", system_messamge)]
        if gpt_mediator is None:
            gpt_mediator = GptMediator(**more_params)
        self.gpt_mediator = gpt_mediator
        self.summarize_all = summarize_all
        self.summarize_last = summarize_last
        self.system_messamge = system_messamge
        self.summary_request = summary_request
        self.full_memory = full_memory
        self.func = func

    def feed_info(self, info):
        ''' receives one bit of info, summarizes if necessary '''
        self.log.append(("user", info))
        if self.summarize_all:
            self.summarize()

    def feed_info_list(self, info_list, pbar=None):
        ''' receives list of info bits, performs summarizations if necessary '''
        if self.summarize_all:
            for info in info_list:
                self.feed_info(info)
                if pbar is not None:
                    pbar.update(1)
        else:
            self.feed_info(" ".join(info_list))
            if pbar is not None:
                pbar.update(len(info_list))

    def summarize(self):
        ''' summarizes either all the info so far, or last summary + last bit of ingo,
        depending on internal settings. Then logs '''
        self.log.append(("user", self.summary_request))
        if self.full_memory:
            summary = self.gpt_mediator.respond(self.log)
        else:
            last_summary_exists = len(self.log) > 3
            include_last = -3 if last_summary_exists else -2
            sublog = [self.log[0]] + self.log[include_last:]
            summary = self.gpt_mediator.respond(sublog)
        self.log.append(("assistant", summary))
        return summary

    def get_opinion(self):
        ''' use gpt_mediator to get the agents opinion '''
        if self.summarize_last and not self.summarize_all:
            self.summarize()
        if self.full_memory:
            o = self.gpt_mediator.function_call(self.log, self.func)
        else:
            sublog = [self.log[0], self.log[-1]]
            o = self.gpt_mediator.function_call(sublog, self.func)
        self.log.append(("assistant", f"opinion = {o}"))
        return o


class Experiment(Saveable):
    ''' social experiment of internal polarization with cusp catastrophe '''
    O_MIN = 0
    O_MAX = 100
    O_BINS = 11

    def __init__(self, database, summarize_all=True, summarize_last=True,
                 full_memory=True, name=None, **more_params):
        self.database = database
        self.name = name

        self.summarize_all = summarize_all
        self.summarize_last = summarize_last
        self.full_memory = full_memory

        self.oo = []
        self.ip = []
        self.im = []
        self.subjects = []

        self.more_params = more_params

    @classmethod
    def init_from(cls, experiments):
        self = cls(experiments[0].database)
        self.summarize_all = experiments[0].summarize_all
        self.summarize_last = experiments[0].summarize_last
        self.full_memory = experiments[0].full_memory
        self.more_params = experiments[0].more_params
        self.name = experiments[0].name

        for exp in experiments:
            self.oo += exp.oo
            self.ip += exp.ip
            self.im += exp.im
            self.subjects += exp.subjects

        return self

    def bistability_test(self, nn=None, iters=10, verbose=True, animate=False):
        ''' nn - list of points along attention axis to test,
        or if int, the single point '''
        preshuffle = True
        preshuffle = False

        if nn is None:
            nn = range(len(self.database))
        elif isinstance(nn, numbers.Number):
            nn = [nn]
        pbar = None
        if verbose:
            pbar = tqdm(total=2*sum(nn)*iters)
        for i in range(iters):
            for n in nn:
                self.one_iteration(n, n, preshuffle=preshuffle, postshuffle=preshuffle,
                                   mix=True, pbar=pbar)
                if animate:
                    plt.clf()
                    if len(nn) > 1:
                        self.hist2d_OvsA()
                    else:
                        self.hist_O(ip=nn[0], im=nn[0])

    def path_dependence_test(self, pro_first, iters=1, animate=False, N=None,
                             verbose=True):
        ''' N - ip and im range '''
        if N is None:
            N = len(self.database)
        pbar = tqdm(total=iters*N*(N+1)**2) if verbose else None

        for _ in range(iters):
            for ip in range(N+1):
                for im in range(N+1):
                    self.one_iteration(ip, im, preshuffle=False, postshuffle=True,
                                       mix=False, pro_first=pro_first, pbar=pbar)
                    if animate:
                        plt.clf()
                        data_plot.contour_plot(self.im, self.ip, self.oo)
                        plt.pause(0.1)

    def one_iteration(self, ip, im, preshuffle, postshuffle, mix, pro_first=True,
                      pbar=None):
        ''' perform one iteration of the test and store the data '''
        info_list = self.database.get_info_list(ip, im, preshuffle=preshuffle,
                                                postshuffle=postshuffle,
                                                mix=mix, pro_first=pro_first)
        subject = ExpSubject(self.summarize_all, self.summarize_last,
                             self.full_memory, self.database.system_messamge,
                             self.database.func,
                             summary_request=self.database.summary_request,
                             **self.more_params
                             )
        subject.feed_info_list(info_list, pbar=pbar)
        o = subject.get_opinion()
        self.oo.append(o)
        self.ip.append(ip)
        self.im.append(im)
        self.subjects.append(subject)

    def hist2d_OvsA(self, bins=[10,10], normalize=True, scatter=True):
        ''' plot the histogram of data points in O A plane'''
        aa = []
        oo = []
        for ip, im, o in zip(self.ip, self.im, self.oo):
            if ip == im:
                aa += [ip*(2**0.5)]
                oo += [o]

        cmap = 'viridis'
        delta = (self.O_MAX-self.O_MIN)/(self.O_BINS-1)
        if normalize:
            # Calculate 2D histogram
            hist, xedges, yedges = np.histogram2d(aa, oo, bins=bins,
                                                  range=[[min(aa), max(aa)],
                                                         [self.O_MIN-delta/2,
                                                          self.O_MAX+delta/2]])

            # Normalize histogram by x values
            hist_normalized = hist / hist.sum(axis=1)[:, None]

            # Plotting
            plt.imshow(hist_normalized.T, origin='lower', aspect='auto',
                       extent=[xedges[0], xedges[-1], yedges[0], yedges[-1]],
                       interpolation='nearest', cmap=cmap, vmin=0, vmax=1)
        else:
            plt.hist2d(aa, oo, bins=bins, range=[[min(aa), max(aa)],
                                                                  [self.O_MIN-delta/2,
                                                                  self.O_MAX+delta/2]],
                       cmap=cmap)
        if scatter:
            plt.scatter(aa, oo, alpha=0.2, marker='.')
        plt.xlabel("attention")
        plt.ylabel("opinion")
        plt.title(self.name)
        colorbar = plt.colorbar()
        colorbar.set_label('probability' if normalize else 'count',
                           rotation=90, labelpad=15)
        plt.tight_layout()
        plt.pause(0.1)

    def hist_O(self, ip, im):
        ''' plot opinion histogram for a given point in information plane '''
        oo = []
        for ip1, im1, o in zip(self.ip, self.im, self.oo):
            if ip == ip1 and im == im1:
                oo += [o]
        delta = (self.O_MAX-self.O_MIN)/(self.O_BINS-1)
        binrange = np.linspace(self.O_MIN-delta/2, self.O_MAX+delta/2, self.O_BINS)
        plt.hist(oo, bins=binrange)
        plt.xlabel("opinion")
        plt.ylabel("count")
        plt.title(self.name)
        plt.tight_layout()
        plt.pause(0.1)

    def __len__(self):
        return len(self.oo)
