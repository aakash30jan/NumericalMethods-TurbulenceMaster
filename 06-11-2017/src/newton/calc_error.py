import pylab as pl
import numpy as np
data_exact=np.loadtxt("xy_exact.dat")
data_poly=np.loadtxt("xy_poly.dat")
#pl.plot(data_exact[:,0],data_exact[:,1],'r',label='exact')
#pl.plot(data_poly[:,0],data_poly[:,1],'bo',label='interpolated')
#pl.show()
er=np.average(np.abs(data_exact[:,1]-data_poly[:,1]))
er
