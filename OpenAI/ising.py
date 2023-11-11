import matplotlib.animation as animation
import matplotlib.cm as cm
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
from tqdm import tqdm
from matplotlib.animation import FuncAnimation
import imageio


class IsingModel:
    """
    This class represents the Ising model on a random graph.
    """

    def __init__(self, n, p):
        """
        Initializes the Ising model on an Erdos-Renyi graph.

        Parameters:
        - n: Number of nodes.
        - p: Probability of an edge between any two nodes.
        """
        self.n = n
        self.p = p
        self.net = nx.erdos_renyi_graph(n, p)
        self.initialize_spins()
        self.pos = nx.spring_layout(self.net)  # Fix the layout

    def initialize_spins(self):
        """
        Initializes the spins of nodes randomly to either +1 or -1.
        """
        for v in self.net.nodes():
            self.net.nodes[v]['spin'] = 1 if np.random.rand() < 0.5 else -1

    def step(self, beta=1.0, h=0.0):
        """
        Performs one Monte Carlo step of the Ising model simulation.

        Parameters:
        - beta: Inverse temperature (1/Temperature)
        - h: External magnetic field
        """
        # Randomly select a node
        v = np.random.choice(list(self.net.nodes()))

        # Compute the change in energy if we flip the spin
        delta_E = 2 * self.net.nodes[v]['spin'] * \
            (sum(self.net.nodes[n]['spin'] for n in self.net.neighbors(v)) + h)

        # Metropolis-Hastings algorithm
        if delta_E < 0 or np.random.rand() < np.exp(-beta * delta_E):
            self.net.nodes[v]['spin'] *= -1

    def visualize_graph(self):
        """
        Visualizes the graph with nodes colored based on their spin.
        """
        colors = ['#FF0000' if self.net.nodes[v]['spin']
                  == -1 else '#0000FF' for v in self.net.nodes()]
        nx.draw(self.net, pos=self.pos, node_color=colors, with_labels=False)
        plt.pause(0.1)

    def simulate(self, beta, steps, h=0.0, visualize=False):
        """
        Simulates the Ising model for a given number of steps.

        Parameters:
        - beta: Inverse temperature (1/Temperature)
        - steps: Number of simulation steps
        - h: External magnetic field
        - visualize: Whether to visualize the graph during simulation

        Returns:
        - Average spin of the graph after the simulation.
        """
        for _ in range(steps):
            self.step(beta, h)
            if visualize:
                self.visualize_graph()
        return self.average_spin()

    def average_spin(self):
        """
        Computes the average spin of nodes in the graph.

        Returns:
        - Average spin.
        """
        return np.mean([self.net.nodes[v]['spin'] for v in self.net.nodes()])


class CuspCatastrophe:
    """
    This class provides utilities to observe the behavior of the Ising model,
    especially pitchfork bifurcation and hysteresis.
    """

    def __init__(self, n, p):
        """
        Initializes the class with the network parameters.

        Parameters:
        - n: Number of nodes for the Ising model's graph.
        - p: Probability of an edge between any two nodes.
        """
        self.n = n
        self.p = p

    def pitchfork(self, betas, steps_per_beta, verbose=False):
        """
        Simulates the Ising model for various values of beta to observe the pitchfork
        bifurcation.

        Parameters:
        - betas: Array of inverse temperature values.
        - steps_per_beta: Number of simulation steps for each beta.
        - verbose: Whether to display progress bar.

        Returns:
        - List of average magnetizations for each beta.
        """
        magnetizations = []
        ising = IsingModel(self.n, self.p)
        for beta in tqdm(betas, disable=not verbose,
                         desc="Simulating Pitchfork Bifurcation"):
            ising.initialize_spins()
            m = ising.simulate(beta, steps_per_beta)
            magnetizations.append(m)
        return magnetizations

    def hysteresis(self, beta, h_values, steps_per_h, verbose=False):
        """
        Simulates the Ising model for various values of external field to observe
        hysteresis.

        Parameters:
        - beta: Fixed inverse temperature value.
        - h_values: Array of external magnetic field values.
        - steps_per_h: Number of simulation steps for each h.
        - verbose: Whether to display progress bar.

        Returns:
        - List of average magnetizations for each external field value during both
        forward and backward sweeps.
        """
        magnetizations = []
        ising = IsingModel(self.n, self.p)

        # Forward sweep
        for h in tqdm(h_values, disable=not verbose,
                      desc="Simulating Hysteresis (Forward)"):
            m = ising.simulate(beta, steps_per_h, h)
            magnetizations.append(m)

        # Backward sweep
        for h in tqdm(h_values[::-1], disable=not verbose,
                      desc="Simulating Hysteresis (Backward)"):
            m = ising.simulate(beta, steps_per_h, h)
            magnetizations.append(m)
        return magnetizations

    def plot(self, x_values, y_values, xlabel, ylabel, title):
        """
        Plots x-values against y-values using scatter plot.

        Parameters:
        - x_values: List of x-values.
        - y_values: List of y-values.
        - xlabel: Label for x-axis.
        - ylabel: Label for y-axis.
        - title: Title of the plot.
        """
        plt.scatter(x_values, y_values, s=1)
        plt.xlabel(xlabel)
        plt.ylabel(ylabel)
        plt.title(title, size=8)

# Example usage is provided in the previous snippet.


n = 1000
pp = np.linspace(0.006, 0.0005, num=12)
beta_max = 1
h_max = 4
points = 500
iters = 10000


# This is just a placeholder for your simulation's plotting logic
i = 0

frames = []
for p in pp:
    plt.clf()  # Clear current plot
    catastrophe = CuspCatastrophe(n, p)

    # To observe the pitchfork bifurcation:
    betas = np.linspace(0.001, beta_max, points)
    magnetizations = catastrophe.pitchfork(betas, iters, verbose=True)
    plt.subplot(1, 2, 1)
    catastrophe.plot(betas, magnetizations, "Beta (1/Temperature)",
                     "Average Spin (Magnetization)",
                     "Pitchfork Bifurcation in the Ising Model")
    plt.ylim([-1, 1])
    plt.pause(0.05)
    # To observe the hysteresis:
    h_values = np.linspace(-h_max, h_max, points)
    magnetizations_hysteresis = catastrophe.hysteresis(
        beta_max, h_values, iters, verbose=True)
    plt.subplot(1, 2, 2)
    catastrophe.plot(list(h_values) + list(h_values[::-1]), magnetizations_hysteresis,
                     "External Field (h)", "Average Spin (Magnetization)",
                     "Hysteresis in the Ising Model")
    plt.suptitle(f"Ising on ER network: $n={n}$, $p={p:.4f}$")
    plt.tight_layout()
    plt.pause(0.05)

    # record the frame
    frame = np.array(plt.gcf().canvas.renderer.buffer_rgba())
    frames.append(frame)


imageio.mimsave("anim2.gif", frames, format='GIF', duration=5, loop=0)
