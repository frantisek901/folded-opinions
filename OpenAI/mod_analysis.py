#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  3 12:00:20 2022

@author: guga
"""
import numpy as np
import scipy
from scipy.interpolate import interp1d
from scipy import stats
from matplotlib import pyplot as plt
from statsmodels.distributions.empirical_distribution import ECDF


class data_analysis:
    @staticmethod
    def hist_from_data(data, log_bins=True, normalized=True, base_bin=1):
        xi = 0.
        xf = max(data)+0.
        if log_bins:
            bins = xi + np.logspace(start=np.log2(1),
                                    stop=np.ceil(np.log2(xf-xi)),
                                    base=2,
                                    num=1+int(np.ceil(np.log2(xf-xi))/base_bin))
        else:
            bins = xi + np.linspace(start=0, stop=xf-xi,
                                    num=int((xf-xi+1)/base_bin))

        yy, _ = np.histogram(data, bins, density=normalized)
        xx = [(bins[i]+bins[i+1])/2 for i in range(len(yy))]
        return xx, yy

    # @staticmethod
    # def hist_from_2d_data(data, log_bins=True, normalized=True, base_bin=1):
    #     xi = 0.;
    #     xf = max(data)+0.;
    #     if log_bins:
    #         bins = xi + np.logspace(start=np.log2(1),
    #                                 stop=np.ceil(np.log2(xf-xi)),
    #                                 base=2,
    #                                 num=1+int(np.ceil(np.log2(xf-xi))/base_bin))
    #     else:
    #         bins = xi + np.linspace(start=0, stop=xf-xi,
    #                                 num=int((xf-xi+1)/base_bin))

    #     yy,_ = np.histogram(data, bins, density=normalized)
    #     xx = [(bins[i]+bins[i+1])/2 for i in range(len(yy))]
    #     return xx, yy

    @staticmethod
    def mean_and_error(data):
        return [np.mean(data), stats.sem(data)]

    @staticmethod
    def means_and_errors(data):
        means = []
        errors = []
        for dt in data:
            m, e = data_analysis.mean_and_error(dt)
            means.append(m)
            errors.append(e)
        return means, errors

    @staticmethod
    def fit_with_poly(xx, yy, deg):
        pp = np.polyfit(xx, yy, deg)
        return poly(pp)

    @staticmethod
    def find_DK(data):
        xx, yy = data_analysis.hist_from_data(data)
        log_xx = []
        log_yy = []
        for i in range(len(xx)):
            if yy[i] > 0:
                log_xx.append(np.log(xx[i]))
                log_yy.append(np.log(yy[i]))

        lin_n = max([i for i in range(len(log_xx)) if
                     log_xx[i] < log_xx[-1]/2])
        line = data_analysis.fit_with_poly(log_xx[0:lin_n], log_yy[0:lin_n],
                                           deg=1)
        line.plot()
        xx, yy = data_analysis.hist_from_data(data, log_bins=True,
                                              base_bin=0.1)
        # data_plot.plot_hist(data, log_bins=True)
        xx = np.log(xx)
        yy = np.log(yy) - line(xx)
        clean_xx = []
        clean_yy = []
        for i in range(len(xx)):
            if yy[i] > 0:
                clean_xx.append(xx[i])
                clean_yy.append(yy[i])
        xx, yy = clean_xx, clean_yy
        # plt.plot(np.exp(xx), np.exp(yy))

        y_dk = max(yy)
        i_dk = yy.index(y_dk)
        x_dk = xx[i_dk]

        i_new = 0
        for i in range(i_dk, 0, -1):
            if yy[i] < y_dk/2 and i_new == 0:
                i_new = i
        # plt.plot(np.exp(xx[i_new:]), np.exp(yy[i_new:]))

        xx = xx[i_new:]
        yy = yy[i_new:]+line(np.array(xx))
        plt.plot(np.exp(xx), np.exp(yy))

        y_dk = max(yy)
        i_dk = list(yy).index(y_dk)
        x_dk = xx[i_dk]

        plt.scatter([np.exp(x_dk)], [np.exp(y_dk)])
        return np.exp(x_dk), np.exp(y_dk)

    @staticmethod
    def DK_heuristic(data, show=False):
        ''' my ad-hoc method of measuring tail deviations'''
        xx, yy = data_analysis.hist_from_data(data)
        log_xx = []
        log_yy = []
        for i in range(len(xx)):
            if yy[i] > 0:
                log_xx.append(np.log(xx[i]))
                log_yy.append(np.log(yy[i]))

        lin_n = int(len(xx)/2)
        line = data_analysis.fit_with_poly(log_xx[0:lin_n],
                                           log_yy[0:lin_n],
                                           deg=1)

        if show:
            data_plot.plot_hist(data)
            line.plot()
        # data_plot.plot_hist(data, log_bins=True)

        xx, yy = data_analysis.hist_from_data(data, base_bin=1)

        clean_xx = []
        clean_yy = []
        for i in range(int(len(xx)*3/4), len(xx)):
            if yy[i] > 0:
                clean_xx.append(np.log(xx[i]))
                clean_yy.append(np.log(yy[i]) - line(clean_xx[-1]))
        xx, yy = clean_xx, clean_yy
        y_dk = max(yy)
        i_dk = list(yy).index(y_dk)
        x_dk = xx[i_dk]

        if show:
            plt.plot(np.exp([x_dk, x_dk]), np.exp([line(x_dk), y_dk+line(x_dk)]))

        return y_dk

    @staticmethod
    def Sornette_u_test(data, show):
        ''' needs checking '''
        xx = np.array(sorted(data, reverse=True))
        print(xx)
        h = 0
        r = 1

        def f(x, b): return b*np.exp(-b*x)
        def F(x, b): return 1-np.exp(-b*(x-h))

        def likelyhood(b): return -(1-F(xx[r+1], b))**r*np.prod(f(xx[r+1:], b))

        # plt.plot(xx, F(xx,1))
        # plt.plot(xx, F(xx,2))

        plt.plot(xx, f(xx, 1))
        plt.plot(xx, F(xx, 0.005))

        print("")
        print(scipy.optimize.minimize(likelyhood, [-1]).x)
        print(likelyhood(1))

    @staticmethod
    def Sornette_DK_test(data, show):
        ''' Sornettes DK test with added negligible noise '''
        xx = np.array(data).astype(float)
        n = len(xx)
        xx += np.random.random(size=n)
        xx.sort()
        xx = xx[::-1]
        # print(xx)
        xx = np.log(xx)

        # th = xx[0]/2
        # r = 0
        # print(xx[0])
        # while xx[r]>th:
        #     r += 1
        # print(f'th={np.exp(th)}, r={r}')

        yy = [x0-x1 for x0, x1 in zip(xx[:-1], xx[1:])]
        yy.append(xx[-1])
        zz = [(k+1)*y for k, y in enumerate(yy)]

        T_max = 0
        p_min = 1
        rr = list(range(1, int(n/2)))
        for r in rr:
            T = np.mean(zz[:r])/np.mean(zz[r:])
            p = 1 - scipy.stats.f.cdf(T, 2*r, 2*(n-r))
            if p < p_min:
                p_min = p
                T_max = T

        if show:
            data_analysis.DK_heuristic(data, True)
        return T_max, p_min

    @staticmethod
    def __Janczura(data, th_up=100, th_dn=10):
        n = len(data)
        sorted_data = np.array(sorted(data))
        ecdf = np.array([1-i/len(sorted_data) for i in range(len(sorted_data))])
        xxx = [sorted_data[0]]
        yyy = [[ecdf[0]]]
        for i in range(1, n):
            if xxx[-1] == sorted_data[i]:
                yyy[-1].append(ecdf[i])
            else:
                yyy.append([ecdf[i]])
            xxx.append(sorted_data[i])
        for i in range(len(yyy)):
            yyy[i] = [np.mean(yyy[i])]*len(yyy[i])
        yyy = [y for yy in yyy for y in yy]
        sorted_data = xxx
        ecdf = yyy

        fit_fraction = 0.85
        th_up = min(np.exp(np.log(max(sorted_data))*fit_fraction), th_up)
        th_i = 0
        th_j = 0
        while sorted_data[th_i] < th_dn:
            th_i += 1
        while sorted_data[th_j] <= th_up:
            th_j += 1
        fit_data = sorted_data[th_i:th_j]

        ecdf_fit = ecdf[th_i:th_j]
        line = data_analysis.fit_with_poly(np.log(fit_data), np.log(ecdf_fit), 1)
        # print(f'line {line}')
        p, c = line.pp
        b = np.exp(c)
        # stdev = line.data_stdev(np.log(fit_data), np.log(ecdf_fit))
        # print(f'STD of linear approximation = {stdev}')

        zzz = np.exp(np.log(ecdf)-np.log(b*sorted_data**p))
        zzz = [(i > th_j)*zzz[i] for i in range(n)]
        dk_i = np.argmax(zzz)
        peak_x = sorted_data[dk_i]
        peak_y = ecdf[dk_i]
        return peak_x, peak_y, b, p, sorted_data, ecdf, ecdf_fit, fit_data

    @staticmethod
    def Janczura_DK_test(data, show=False, alpha=0.05, th_up=100, th_dn=10):
        n = len(data)
        peak_x, peak_y, b, p, sorted_data, ecdf, ecdf_fit, fit_data =\
            data_analysis.__Janczura(data, th_up, th_dn)

        z1 = stats.norm.ppf(alpha/2)
        z2 = stats.norm.ppf(1-alpha/2)
        def f_dn(x): return b*x**p + z1*np.sqrt(b*x**p/n*(1-b*x**p))
        def f_up(x): return b*x**p + z2*np.sqrt(b*x**p/n*(1-b*x**p))

        if show:
            plt.plot(sorted_data, ecdf)
            plt.plot(sorted_data, b*sorted_data**p)
            if alpha == 0.05:
                plt.plot(fit_data, ecdf_fit, '.')  # data to which
            if alpha <= 0.05:
                plt.scatter([peak_x], [peak_y], color='red')
                plt.plot(sorted_data, f_up(sorted_data), color='gray', linewidth=0.5)
                plt.plot(sorted_data, f_dn(sorted_data), color='gray', linewidth=0.5)

        return f_dn(peak_x) > peak_y

    @staticmethod
    def Janczura_p_val(data, th_up=100, th_dn=10):
        n = len(data)
        try:
            peak_x, peak_y, b, p, sorted_data, ecdf, ecdf_fit, fit_data =\
                data_analysis.__Janczura(data, th_up, th_dn)
        except:
            return 1.
        z2 = (peak_y-b*peak_x**p)/np.sqrt(b*peak_x**p/n*(1-b*peak_x**p))
        z2 = max(0, z2)

        alpha = 2.-2*stats.norm.cdf(z2)
        return alpha


class poly:
    def __init__(self, pp):
        ''' highest degree coefficient first '''
        self.pp = pp

    def __getitem__(self, i):
        return self.pp[i]

    def __call__(self, x):
        ''' evaluate at x '''
        return sum([x**(self.deg()-i)*self[i] for i in range(self.deg()+1)])

    def deg(self):
        return len(self.pp)-1

    def plot(self, x0=None, x1=None, legend=None):
        if x0 is None:
            x0 = plt.gca().get_xlim()[0]
        if x1 is None:
            x1 = plt.gca().get_xlim()[1]

        if plt.gca().get_xscale() == "log":
            if x0 == 0:
                x0 = 0.01
            xx = np.logspace(np.log(x0), np.log(x1), 100, base=np.e)
            plt.plot(xx, np.exp(self(np.log(xx))), label=legend)
        else:
            xx = np.linspace(x0, x1, 100)
            plt.plot(xx, self(xx), label=legend)

    def x_extremal(self):
        if self.deg() == 2:
            return -self[1]/(2*self[0])
        else:
            raise Exception('not implemented')

    def data_stdev(self, xx, yy):
        zz = np.array([self(x) for x in xx])
        zz -= np.array(yy)
        return np.linalg.norm(zz)/np.sqrt(len(zz))

    def __str__(self):
        st = '$'
        for i in range(len(self.pp)):
            if i > 0:
                st += ' '
                if self[i] > 0:
                    st += '+'
            st += f'{self[i]:.2f}·x^{self.deg()-i}'
        st += '$'
        return st

    def __repr__(self):
        return str(self)


class data_plot:
    @staticmethod
    def plot_hist(data, label=None, log_bins=True, normalized=True, base_bin=1,
                  **kwargs):
        xx, yy = data_analysis.hist_from_data(data, log_bins=log_bins,
                                              normalized=normalized,
                                              base_bin=base_bin)
        plt.plot(xx, yy, '.-', label=label, **kwargs)
        plt.gca().set_xscale("log")
        plt.gca().set_yscale("log")
        plt.show()
        return xx, yy

    @staticmethod
    def plot_fit(xx, yy, deg=1, xmin=-np.infty, xmax=np.infty):
        imin = 0
        imax = len(xx)-1
        for i in range(len(xx)):
            if xx[i] < xmin:
                imin = i+1
            if xx[-i] > xmax:
                imax = len(xx)-i-1
        p = data_analysis.fit_with_poly(np.log(xx[imin:imax]),
                                        np.log(yy[imin:imax]), deg)
        p.plot(xx[imin], xx[imax], legend=str(p))
        plt.legend()

        return p

    @staticmethod
    def plot_log_slope(x0, x1, a=-3/2):
        poly([-a, 0]).plot()

    @staticmethod
    def contour_plot(xx, yy, zz, lvls=30,  labels=[None, None, None],
                     extend_up_down=(False, False), ticks=None,
                     colors='summer', show_data_points=False):
        extend = 'neither'
        if extend_up_down[0]:
            extend = 'max'
        if extend_up_down[1]:
            extend = 'min'
        if extend_up_down[0] and extend_up_down[1]:
            extend = 'both'

        plt.tricontourf(xx, yy, zz, lvls,  # np.arange(0, 0.1, 0.1/lvls),
                        extend=extend, cmap=colors)

        if ticks is not None:
            cbar = plt.colorbar(label=labels[2], ticks=ticks)
            try:
                cbar.ax.set_xticklabels([str(tick) for tick in ticks])
            except:
                pass
        else:
            cbar = plt.colorbar(label=labels[2])

        plt.xlabel(labels[0])
        plt.ylabel(labels[1])
        if show_data_points:
            plt.scatter(xx, yy, marker='.', facecolors='k', s=0.1)
        return cbar

    @staticmethod
    def pixel_plot_2d(xx, yy, zz, lvls=30,  labels=[None, None, None],
                      extend_up_down=(False, False), ticks=[0, 1],
                      colors='summer', show_data_points=False):

        xx_bins = np.array(sorted(list(set(xx))))
        np.append(xx_bins, 2*xx_bins[-1] - xx_bins[-2])
        xx_bins -= (xx_bins[1]-xx_bins[0])/2

        yy_bins = np.array(sorted(list(set(yy))))
        np.append(yy_bins, 2*yy_bins[-1] - yy_bins[-2])
        yy_bins -= (yy_bins[1]-yy_bins[0])/2

        plt.hist2d(xx, yy, weights=zz, cmap=colors, bins=[xx_bins, yy_bins],
                   vmin=ticks[0], vmax=ticks[-1])

        if ticks is not None:
            cbar = plt.colorbar(label=labels[2], ticks=ticks)
            try:
                cbar.ax.set_xticklabels([str(tick) for tick in ticks])
            except:
                pass
        else:
            cbar = plt.colorbar(label=labels[2])

        plt.xlabel(labels[0])
        plt.ylabel(labels[1])
        if show_data_points:
            plt.scatter(xx, yy, marker='.', facecolors='k', s=0.1)

    @staticmethod
    def paper_plot_prep_1(with_scilimits=True):
        plt.rcParams['font.size'] = 14
        plt.rcParams['axes.linewidth'] = 1
        fig = plt.figure(figsize=(4, 2.5))
        if with_scilimits:
            plt.ticklabel_format(style='sci', axis='both', scilimits=(-1, 2))
        # plt.figure(fig.number)
        return fig

    @staticmethod
    def paper_plot_prep_2(with_scilimits=True):
        plt.rcParams['font.size'] = 14
        plt.rcParams['axes.linewidth'] = 1
        fig = plt.figure(figsize=(2, 3))
        if with_scilimits:
            plt.ticklabel_format(style='sci', axis='both', scilimits=(0, 0))
        # plt.figure(fig.number)
        return fig

    @staticmethod
    def paper_plot_prep_3(with_scilimits=True):
        plt.rcParams['font.size'] = 16
        plt.rcParams['axes.linewidth'] = 1
        fig = plt.figure(figsize=(3.5, 2.5))
        if with_scilimits:
            plt.ticklabel_format(style='sci', axis='both', scilimits=(0, 3))
        # plt.figure(fig.number)
        return fig

    @staticmethod
    def paper_plot_prep_4(with_scilimits=True):
        plt.rcParams['font.size'] = 16
        plt.rcParams['axes.linewidth'] = 1
        fig = plt.figure(figsize=(3.5, 3.5))
        if with_scilimits:
            plt.ticklabel_format(style='sci', axis='both', scilimits=(0, 2))
        # plt.figure(fig.number)
        return fig

    @staticmethod
    def save_eps(name='blank'):
        plt.savefig(name+'.eps', format='eps')
