import numpy as np
import pylab as pl
p1=np.loadtxt('eq1_7a.dat')
p2=np.loadtxt('eq1_7b.dat')
p3=np.loadtxt('eq2_7a.dat')
p4=np.loadtxt('eq2_7a.dat')


pl.plot(p1[:,0],p1[:,1],'r',label='exact')
pl.plot(p1[:,0],p1[:,2],'bo-',label='non-central, 1st')
pl.plot(p2[:,0],p2[:,2],'gs-',label='central, 2nd')
pl.title(r'For Equation 1 with $\Delta x=0.02$')
pl.xlabel('x')
pl.ylabel('Second Derivative')
pl.legend()
pl.show()

pl.plot(p3[:,0],p3[:,1],'r',label='exact')
pl.plot(p3[:,0],p3[:,2],'bo-',label='non-central, 1st')
pl.plot(p4[:,0],p4[:,2],'gs-',label='central, 2nd')
pl.title(r'For Equation 2 with $\Delta x=0.02$')
pl.xlabel('x')
pl.ylabel('Second Derivative')
pl.legend()
pl.show()


d1=np.loadtxt('eq1_8_centraldiff.dat')
#slope, intercept = np.polyfit(np.log10(1/d1[:,0], np.log10(d1[:,1]), 1))
#pl.loglog(1/d1[:,0],d1[:,1],'ro-',label='slope='+str(slope+1)) 
pl.title(r'Plot of Mean Error with $\Delta x$')
pl.xlabel(r'$\Delta x$')
pl.ylabel('Mean Error')
pl.legend()
pl.show()
