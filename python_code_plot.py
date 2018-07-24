### this code is written for renmeng's project to make a performance plot.

import numpy as np 
import matplotlib.pyplot as plt  
from os.path import abspath


## the raw data from the txt file

x = [0,0.2,0.5,0.6,0.7,0.8,0.9]   #  the x axis values
y = [0.886,0.886,0.938,0.948,0.967,0.952,0.957]   # the y axis values
std=[0.031,0.026,0.013,0.031,0.027,0.017,0.026]   #  the y error values

## make the plot

# adjust some fonts and parameters
SMALL_SIZE = 8
MEDIUM_SIZE = 12
BIGGER_SIZE = 20

plt.rc('font', size=BIGGER_SIZE)          # controls default text sizes
plt.rc('axes', titlesize=BIGGER_SIZE)     # fontsize of the axes title
plt.rc('axes', labelsize=BIGGER_SIZE)    # fontsize of the x and y labels
plt.rc('xtick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('ytick', labelsize=MEDIUM_SIZE)    # fontsize of the tick labels
plt.rc('legend', fontsize=SMALL_SIZE)    # legend fontsize

# plot
plt.figure()
plt.xlim (-0.1, 1.0)
plt.xlabel('Missing value percentage (100%)', labelpad=5)
plt.ylim (0.2, 1.0)
plt.ylabel('Performance (100%)', labelpad=10)
plt.errorbar(x, y, yerr=std, fmt='.r-', markersize=16, capsize=5, elinewidth=2, linewidth=3)
plt.title ('Neural network prediction accuracy', y=1.03)
plt.show()


