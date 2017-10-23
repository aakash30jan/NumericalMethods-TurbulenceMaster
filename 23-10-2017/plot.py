import numpy as np
data=np.loadtxt("prob_4data.dat")
data.shape
import pylab as pl
pl.plot(data[:,0],data[:,2])
pl.plot(data[:,0],data[:,4])

pl.show()

