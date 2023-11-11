#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul  1 16:16:41 2020

@author: guga
"""
# import pickle
import dill as pickle
import matplotlib.pyplot as plt
from pympler import asizeof


class Saveable:
    def save(self, name=None):
        ''' save object to file in folder \Data '''
        if name == None:
            name = str(self)
        name = name.replace('/', '_')
        file_pi = open("Data/" + name.replace('\n', ' '), 'wb')
        pickle.dump(self, file_pi)

    @staticmethod
    def load(name):
        ''' read sandpile object from file from folder \Data '''
        filehandler = open("Data/" + name.replace('\n', ' '), 'rb')
        return pickle.load(filehandler)

    def memory_usage(self):
        return asizeof.asizeof(self)/1000000

    def __repr__(self):
        return str(self)
