import numpy as np
from workhorse import WorkHorse
import random
import openai
import time
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import os

openai.api_key = open(os.path.expanduser("~")+"/.openai_api_key", "r").read()


class ExpSubject:
    ITS_OK = """Imitate a social study experiment subject. 
    You will be given some information about a purely hypothetical person called Dave 
    Smith. Then you will be asked to assess his virtue to the best of your capability
    on the scale from 0 to 100. Your score should roughly match the percentile of Dave 
    Smith in the human population."""
    # With every prompt, you will receive one or more of
    # the following instructions: '''think''' means that you have to assess and
    # conceptualize the provided information; '''assess''' means that you have to respond
    # with a function call to register your numerical assessment; '''text''' means that
    # you will have to answer a specific question provided in the prompt."""

    GOOD = [
        "Dave Smith has volunteered at a local homeless shelter a few times in the past.",
        "Dave Smith has shown some potential for leadership skills in minor projects at work.",
        "Dave Smith enjoys playing music as a hobby and occasionally performs at casual gatherings.",
        "Dave Smith maintains an acceptable academic performance, meeting the basic requirements of his studies.",
        "Dave Smith occasionally participates in community clean-up drives to contribute to a cleaner environment.",
        "Dave Smith has offered occasional advice to individuals when available.",
        "Dave Smith is willing to lend an ear and provide support to friends and family when possible.",
        "Dave Smith sometimes donates to charitable organizations, showing occasional support for causes he cares about.",
        "Dave Smith cooperates with his team at work, fulfilling his responsibilities adequately.",
        "Dave Smith has an interest in cooking and occasionally prepares meals for friends and family to enjoy."
    ]

    BAD = [
        "Dave Smith has frequently been late to meetings, causing significant inconvenience to colleagues and disrupting the schedule.",
        "Dave Smith consistently interrupts others during conversations, often disregarding their thoughts and opinions.",
        "Dave Smith has accumulated numerous traffic citations for various offenses, including reckless driving and running red lights.",
        "Dave Smith frequently procrastinates, often resulting in rushed and subpar submissions of assignments or tasks.",
        "Dave Smith struggles to control anger in stressful situations, leading to frequent outbursts and confrontations.",
        "Dave Smith habitually exaggerates his achievements and experiences when sharing stories, often distorting the truth.",
        "Dave Smith has a history of borrowing items from friends and acquaintances without returning them or acknowledging the act.",
        "Dave Smith lives in a consistently disorganized and cluttered personal space, making it difficult to maintain order.",
        "Dave Smith frequently cancels plans on short notice, displaying a lack of consideration for others' time and commitments.",
        "Dave Smith regularly ignores or forgets to respond to text messages and emails, causing communication gaps and misunderstandings."
    ]

    functions = [{
        "name": "submit_virtue_assessment",
        "description": "submits an assessment of Dave Smiths virtue",
        "parameters": {
            "type": "object",
            "properties": {
                "virtue_assessment": {
                    "type": "number",
                    "description": "This parameter holds the assessed value of Dave Smiths virtue between 0. and 10.",
                    # "enum": [0,1,2,3,4,5]
                },
            },
            "required": ["virtue_assessment"],
        },
    }]

    def __init__(self, model="gpt-3.5-turbo"):
        self.model = model
        self.temp = 0.7

    def respond(self, messages):
        # messages = [{"role": "user", "content": prompt}]
        response = openai.ChatCompletion.create(
            model=self.model,
            messages=messages,
            temperature=self.temp,
            functions=self.functions
        )
        return response  # .choices[0].message["content"]

    def function_call(self, messages):
        # messages = [{"role": "user", "content": prompt}]
        response = openai.ChatCompletion.create(
            model=self.model,
            messages=messages,
            temperature=self.temp,
            functions=self.functions,
            function_call={"name": "submit_virtue_assessment"}
        )
        return eval(response.choices[0].message["function_call"]["arguments"])["virtue_assessment"]

    @classmethod
    def get_messages_exp2(cls, n):
        '''testing bistability:
        get messages with n positive and n negative pieces, randomly mixed'''
        good = cls.GOOD[:n]
        bad = cls.BAD[:n]
        sentences = good + bad
        random.shuffle(sentences)
        content = " ".join(sentences)
        msgs = [{"role": "system", "content": cls.ITS_OK}]
        msgs += [{"role": "user", "content": content}]
        return msgs

    @classmethod
    def get_messages_exp1(cls, sequence, shuffle_within_group=True):
        '''testing path dependence:
        get messages with the chunks of positive and negative pieces of info, 
        as specified in "sequences". positive number stands for bits of positive info,
        negative number indicates the number of negative bits'''
        msgs = [{"role": "system", "content": cls.ITS_OK}]
        counter_m = 0
        counter_p = 0
        good = cls.GOOD
        bad = cls.BAD
        if shuffle_within_group:
            random.shuffle(good)
            random.shuffle(bad)
        content = ""
        for i in sequence:
            for j in range(abs(i)):
                if i < 0:
                    content += " " + bad[counter_m]
                    counter_m += 1
                else:
                    content += " " + good[counter_p]
                    counter_p += 1
        # content += " '''assess''' "
        msgs += [{"role": "user", "content": content}]
        return msgs


def f_exp1(pp, _1=None, _2=None):
    '''experiment with llm getting info in positive and negative chunks'''
    ip, im, first_bad = pp
    sequence = [-im, ip] if first_bad else [ip, -im]
    msgs = ExpSubject.get_messages_exp1(sequence)
    incomplete = True
    while incomplete:
        try:
            subject = ExpSubject()
            response = subject.function_call(msgs)
            incomplete = False
            time.sleep(0.5)
        except Exception:
            print("retrying")
            time.sleep(1)
    return response


def f_exp2(n, _1=None, _2=None):
    '''experiment with llm getting info in positive and negative chunks'''
    msgs = ExpSubject.get_messages_exp2(n)
    incomplete = True
    while incomplete:
        try:
            subject = ExpSubject()
            response = subject.function_call(msgs)
            incomplete = False
            time.sleep(0.1)
        except Exception:
            print("retrying")
            time.sleep(1)
    return response


def run_exp1():
    params = []
    for first_bad in [True, False]:
        for i in range(10):
            for j in range(10):
                params.append([i, j, first_bad])

    wh = WorkHorse(f_exp1, params, threads=1, iters=10)
    wh.compute_data()
    wh.compute_means_and_errors()
    wh.save("10x10 (10 iters).wh")


def run_exp2():
    params = list(range(10, 11))  # [10]  #
    iters = 10
    wh = WorkHorse(f_exp2, params, threads=1, iters=iters)
    wh.compute_data()
    # wh.save(f"exp2 no_judgement range={min(params)}-{max(params)} (500 iters).wh")
    # wh = WorkHorse.load("exp2 no_judgement range=1-10 (500 iters).wh")
    # wh = WorkHorse.load("exp2 no_judgement range=10-10 (500 iters).wh")

    nn = []
    oo = []
    for n, ops in zip(wh.pp, wh.data):
        for o in ops:
            nn += [n]
            oo += [o]

    plt.hist2d(nn, oo, bins=[10, 100], range=[[min(wh.pp), max(wh.pp)], [0, 100]])
    plt.xlabel("attention")
    plt.ylabel("opinion")


if __name__ == "__main__":
    # run_exp1()
    run_exp2()
