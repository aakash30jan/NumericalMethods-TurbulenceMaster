import pylab as pl
import numpy as np
#data_exact=np.loadtxt("xy_exact.dat")
#data_poly=np.loadtxt("xy_poly.dat")
#pl.plot(data_exact[:,0],data_exact[:,1],'r',label='exact')
#pl.plot(data_poly[:,0],data_poly[:,1],'bo',label='interpolated')
#pl.show()
#er=np.average(np.abs(data_exact[:,1]-data_poly[:,1]))
#print er
data=np.loadtxt("eq2.dat")
N=np.log10(data[0:6,0])  #discritization points
er4=np.log10(data[0:6,1]) #error for order 4
er9=np.log10(data[6:12,1]) #error for order 9 
pl.xlim(0,7)
pl.ylim(0,-7)
pl.xlabel('Number of Discritization Points (log10)')
pl.ylabel('Mean Error (log10)')
pl.title('Error in Interpolation (Newton Divided Polynomial)')
pl.plot(N,er4,'ro-',label='4th Order')
pl.plot(N,er9,'bo-',label='9th Order')
pl.legend()
pl.savefig('eq2.png')
pl.show()

