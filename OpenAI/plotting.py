import numpy as np
from workhorse import WorkHorse
import matplotlib.pyplot as plt
import matplotlib.tri as mtri
from mod_analysis import data_plot
from matplotlib.tri import Triangulation


# plot data
wh = WorkHorse.load("10x10 (10 iters).wh")
ipip1 = []  # first bad
imim1 = []
oo1 = []
oeoe1 = []
ipip2 = []  # first good
imim2 = []
oo2 = []
oeoe2 = []
for pp, o, oe in zip(wh.pp, wh.means, wh.errors):
    ip, im, first_bad = pp
    if im == 9 and ip == 9:
        continue
    if first_bad:
        ipip1.append(ip)
        imim1.append(im)
        oo1.append(o)
        oeoe1.append(oe)
    else:
        ipip2.append(ip)
        imim2.append(im)
        oo2.append(o)
        oeoe2.append(oe)


def plot_3d_scatter():
    # Create a 3D plot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    # Plot the scatter points
    ax.scatter(imim1, ipip1, oo1, label="bad first")
    # Plot the error bars along the z-axis
    ax.errorbar(imim1, ipip1, oo1, zerr=oeoe1, fmt='.')

    # Plot the scatter points
    ax.scatter(imim2, ipip2, oo2, label="good first")
    # Plot the error bars along the z-axis
    ax.errorbar(imim2, ipip2, oo2, zerr=oeoe2, fmt='.')

    # Set labels and title
    ax.set_xlabel('$I^+$')
    ax.set_ylabel('$I^-$')
    ax.set_zlabel('O')
    # ax.set_title('3D Scatter Plot with Error Bars')

    # Show the plot
    plt.show()
    plt.legend()


def plot_3d_surface():
    # Create a 3D plot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    tri = Triangulation(imim1, ipip1)

    ax.plot_trisurf(imim1, ipip1, oo1, triangles=tri.triangles,
                    linewidths=0.2, alpha=0.5, label="negative info. first")

    ax.plot_trisurf(imim2, ipip2, oo2, triangles=tri.triangles,
                    linewidths=0.2, alpha=0.5, label="positive info. first")

    # Set labels and title
    ax.set_xlabel('$I^+$')
    ax.set_ylabel('$I^-$')
    ax.set_zlabel('O')
    # ax.set_title('3D Scatter Plot with Error Bars')

    # Show the plot
    plt.show()
    plt.legend()


def plot_differences_3d():
    # Create a 3D plot
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    oodif = np.array(oo2)-np.array(oo1)
    oodifer = np.array(oeoe1)+np.array(oeoe2)
    # ax.scatter(imim1, ipip1, oodif)
    # ax.errorbar(imim1, ipip1, oodif,
    #             zerr=oodifer, fmt='.')

    tri = Triangulation(imim1, ipip1)

    # ax = plt.axes(projection='3d')
    ax.plot_trisurf(imim1, ipip1, oodif, triangles=tri.triangles,
                    cmap='viridis', linewidths=0.2)

    ax.plot_trisurf(imim1, ipip1, oodif+oodifer, triangles=tri.triangles,
                    cmap='viridis', linewidths=0.2, alpha=0.1)

    ax.plot_trisurf(imim1, ipip1, oodif-oodifer, triangles=tri.triangles,
                    cmap='viridis', linewidths=0.2, alpha=0.1)

    # Show the plot
    plt.show()
    ax.set_xlabel('$I^-$')
    ax.set_ylabel('$I^+$')
    ax.set_zlabel('Path dependence of O')
    # plt.legend()


def contour_plot():
    cbar = data_plot.contour_plot(imim1, ipip1, np.array(oo2)-np.array(oo1))
    plt.xticks(rotation=135)
    plt.yticks(rotation=135)
    plt.xlabel("$I^+$", rotation=135)
    plt.ylabel("$I^-$", rotation=135)
    cbar.set_label('path dependence of opinion', rotation=90)
    plt.show()
    plt.tight_layout()


# plot_3d()
# contour_plot()
# plot_differences_3d()
# plot_3d_surface()
plot_3d_scatter()
