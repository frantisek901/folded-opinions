import numpy as np
import matplotlib.pyplot as plt

# Sample data
xx = np.random.rand(1000)
yy = np.random.rand(1000)
yy[:100] *= 2

# Calculate 2D histogram
hist, xedges, yedges = np.histogram2d(xx, yy, bins=(10, 10))

# Normalize histogram by x values
hist_normalized = hist / hist.sum(axis=1)[:, None]

# Plotting
plt.imshow(hist_normalized.T, origin='lower', aspect='auto',
           extent=[xedges[0], xedges[-1], yedges[0], yedges[-1]],
           interpolation='nearest', cmap='viridis', vmin=0, vmax=1)

plt.colorbar(label="Normalized Counts")
plt.xlabel("x")
plt.ylabel("y")
plt.title("2D Histogram Normalized for x")
plt.show()
