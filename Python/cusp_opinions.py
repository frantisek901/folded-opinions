import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.tri as tri
from scipy.spatial import Delaunay
import random
from tqdm import tqdm
import imageio


class FoldedLandscape:
    def __init__(self, folding_point):
        self.folding_point = folding_point

    def do(self, im, ip, o):
        ''' return the derivative of o '''
        return -o**3 + (im+ip-self.folding_point)*o - (im-ip)

    def get_stationary_opinions(self, im, ip):
        ''' ip and im are the positive and negative informations.
        The function returns the stationary values of opinion '''
        # find the roots of the cubic equation: opinion derivative = 0
        a_min = self.folding_point
        roots = np.roots([-1, 0, (ip+im-a_min), (ip-im)])
        # eliminate the complex solutions
        roots = [np.real(root) for root in roots if not np.iscomplex(root)]
        return roots

    def plot_3d(self, i_max=5):
        ''' create a 3d plot of the cusp catastrophe where the information axes extend
        from 0 to i_max'''
        if not hasattr(self, "ax"):
            plot_resolution = 60
            im_list = []
            ip_list = []
            o_list = []
            for im in np.linspace(0, i_max, plot_resolution):
                for ip in np.linspace(0, i_max, plot_resolution):
                    opinions = self.get_stationary_opinions(im, ip)
                    for o in opinions:
                        im_list.append(im)
                        ip_list.append(ip)
                        o_list.append(o)
            im_list = np.array(im_list)
            ip_list = np.array(ip_list)
            o_list = np.array(o_list)
            tri = Delaunay(list(zip(im_list+ip_list, o_list)))
            triangles = []
            max_im = max(im_list)
            max_ip = max(ip_list)
            for i, j, k in tri.simplices:
                not_on_cutoff = ((im_list[i] != max_im or
                                  im_list[j] != max_im or
                                  im_list[k] != max_im) and
                                 (ip_list[i] != max_ip or
                                  ip_list[j] != max_ip or
                                  ip_list[k] != max_ip) and
                                 (im_list[i] != 0 or
                                  im_list[j] != 0 or
                                  im_list[k] != 0) and
                                 (ip_list[i] != 0 or
                                  ip_list[j] != 0 or
                                  ip_list[k] != 0))
                if not_on_cutoff:
                    triangles.append((i, j, k))

            fig = plt.gcf()
            self.ax = fig.add_subplot(111, projection='3d')
            self.im_list = im_list
            self.ip_list = ip_list
            self.o_list = o_list
            self.triangles = triangles

        self.ax.plot_trisurf(self.im_list, self.ip_list, self.o_list,
                             triangles=self.triangles, linewidth=0, alpha=0.3,
                             color="darkgreen")
        # Setting the labels for the axes
        self.ax.set_xlabel('$I^-$')
        self.ax.set_ylabel('$I^+$')
        self.ax.set_zlabel('O')


class Agent:
    DT = 0.01
    MAX_PACKAGE = 1

    def __init__(self, landscape, forgetting, im, ip, o=0):
        self.landscape = landscape
        self.forgetting = forgetting
        self.im = im
        self.ip = ip
        self.o = 0
        self.relax()

    @classmethod
    def random(cls, landscape, forgetting, im_range, ip_range):
        return cls(landscape, forgetting,
                   random.uniform(*im_range), random.uniform(*ip_range))

    def relax(self):
        ''' converges the opinion to the surface '''
        possible_opinions = self.landscape.get_stationary_opinions(self.im, self.ip)
        if len(possible_opinions) == 1:
            self.o = possible_opinions[0]
        else:
            print(possible_opinions)
            possible_opinions = sorted(possible_opinions)
            if self.o > possible_opinions[1]:
                self.o = possible_opinions[2]
            else:
                self.o = possible_opinions[0]

    def forget(self):
        self.ip -= self.ip * self.forgetting * self.DT
        self.im -= self.im * self.forgetting * self.DT

    def adapt_opinion(self):
        self.o += self.landscape.do(self.im, self.ip, self.o) * self.DT

    def listen_to(self, other):
        if other.o > 0:
            self.ip += (other.ip-self.ip) * self.DT
        else:
            self.im += (other.im-self.im) * self.DT


class Society:
    SKIP_FRAMES = 1
    def __init__(self, landscape, agents, com_rate):
        self.landscape = landscape
        self.agents = agents
        self.com_rate = com_rate

        self.frames = []  # for saving an animation

    @classmethod
    def black_pete(cls, landscape, num_of_neutrals, num_of_activists,
                   com_rate, forgetting):
        agents = [Agent.random(landscape, forgetting, (0, 0), (0, 2))
                  for i in range(num_of_neutrals)]
        agents += [Agent.random(landscape, forgetting, (4, 5), (0., 1))
                   for i in range(num_of_activists)]
        return cls(landscape, agents, com_rate)

    def one_round(self):
        # forgetting and opinion update
        for agent in self.agents:
            agent.forget()
            agent.adapt_opinion()

        # interactions
        for i in range(self.com_rate):
            ag1 = self.random_speaker()
            ag2 = self.random_listener()

            ag2.listen_to(ag1)
            ag1.listen_to(ag2)

    def random_speaker(self):
        probabilities = np.array([agent.ip+agent.im for agent in self.agents])
        probabilities /= sum(probabilities)
        return np.random.choice(self.agents, 1, p=probabilities)[0]

    def random_listener(self):
        return random.choice(self.agents)

    def run(self, iterations, verbose=True, animate=True):
        for i in tqdm(range(iterations)) if verbose else range(iterations):
            self.one_round()
            if animate and i % self.SKIP_FRAMES == 0:
                if i == 0:
                    self.landscape.plot_3d()
                self.scatter()
                plt.pause(0.01)

    def scatter(self):
        if hasattr(self, "scatter_plot"):
            self.scatter_plot.remove()
        im = [agent.im for agent in self.agents]
        ip = [agent.ip for agent in self.agents]
        o = [agent.o for agent in self.agents]
        self.scatter_plot = self.landscape.ax.scatter(im, ip, o, c=-np.array(o),
                                                      cmap='coolwarm', alpha=1,
                                                      edgecolors='k', linewidth=0.5,
                                                      vmin=-1, vmax=1)
        self.snap_a_pic()

    def snap_a_pic(self):
        ''' record the current frame '''
        fig = plt.gcf()
        fig.canvas.draw()
        frame = np.array(fig.canvas.renderer.buffer_rgba())
        self.frames.append(frame)

    def export_gif(self, file_name, fps=10):
        imageio.mimsave(file_name+".gif", self.frames, format='GIF', duration=1000/fps,
                        loop=0)

    def export_mp4(self, file_name, fps=10):
        writer = imageio.get_writer(file_name+".mp4", fps=fps)
        for frame in self.frames:
            writer.append_data(frame)
        writer.close()



folding_point = 1.
num_of_neutrals = 50
num_of_activists = 5
com_rate = 15
forgetting = 0.

skip_frames = 1

landscape = FoldedLandscape(folding_point)
society = Society.black_pete(landscape, num_of_neutrals, num_of_activists,
                             com_rate, forgetting)
society.SKIP_FRAMES = skip_frames

society.run(10000)


society.export_mp4("black pete", fps = 30)