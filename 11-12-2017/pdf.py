
import netCDF4 as nc
d1=nc.Dataset("./dataHWilkay/10kHz/10khz_300k_005cm.nc",'r',format="NETCDF4")
#In [5]: d1.data_model
#Out[5]: 'NETCDF3_CLASSIC'
#d1.variables  #this will yell out the stuff

jetHW=d1.variables['jetHW'][:]
time=d1.variables['time'][:]
jetHW__time=d1.variables['jetHW__time'][:]


import pylab as pl
import numpy as np


mean=np.mean(jetHW)
fluctuations=jetHW-mean

import scipy.stats
import matplotlib.pyplot as plt 

#plotting a histogram
hist, bins = np.histogram(fluctuations, bins=fluctuations.shape[0], normed=True)
bin_centers = (bins[1:]+bins[:-1])*0.5
plt.plot(bin_centers, hist)



# test values for the bw_method option ('None' is the default value)
bw_values =  [None, 0.1, 0.01]
bw_values =  [None]

# generate a list of kde estimators for each bw
kde = [scipy.stats.gaussian_kde(fluctuations,bw_method=bw) for bw in bw_values]


# plot (normalized) histogram of the data
plt.hist(fluctuations, bins=100, normed=1, facecolor='green', alpha=0.5);

# plot density estimates
t_range = time
for i, bw in enumerate(bw_values):
    plt.plot(t_range,kde[i](t_range),lw=2, label='bw = '+str(bw))
plt.legend(loc='best')
