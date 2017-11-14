import pylab as pl
import numpy as np
data=np.loadtxt("eqn1.dat")
N=np.log10(data[0:5,0])  #discritization points
er4=np.log10(data[0:5,1]) #error for order 4
er9=np.log10(data[5:10,1]) #error for order 9 
pl.xlim(0,6)
pl.ylim(0,-9)
pl.xlabel('Number of Discritization Points (log10)')
pl.ylabel('Mean Error (log10)')
pl.title('Error in Interpolation (Lagrange Polynomial)')
pl.plot(N,er4,'ro-',label='4th Order')
pl.plot(N,er9,'bo-',label='9th Order')
pl.legend()
pl.savefig('eq1.png')
pl.show()


#plot curves
#import pylab as pl
#import numpy as np
#data=np.loadtxt("eqn1.dat")
#pl.plot(data[:,0],data[:,1],'r')
#pl.plot(data[:,0],data[:,3],'bo')
#pl.show()
