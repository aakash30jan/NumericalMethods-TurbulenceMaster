import numpy as np
files=['h_Err_gaussSiedel.dat','h_Err_gaussSiedelRelaxed.dat','h_Err_jacobian.dat'] 

#for i in range(0,3):
#        filename=files[i]
#        data1=np.loadtxt(filename)

data1=np.loadtxt(files[0])

data2=np.loadtxt(files[1])

data3=np.loadtxt(files[2])

import pylab as pl
# h vs norm2
pl.loglog(data1[:,0],data1[:,2],'kx-',label='gaussSiedel')
pl.loglog(data2[:,0],data2[:,2],'ko-',label='gaussSiedelRelaxed')
pl.loglog(data3[:,0],data3[:,2],'ks-',label='jacobi')
pl.title('h versus norm2 error')
pl.xlabel('h')
pl.ylabel('norm2 error')
pl.legend()
pl.show()

#data4=temp
slope, intercept = np.polyfit(np.log10(data4[:,0]), np.log10(data4[:,2]), 1)
slope=str(slope)
pl.loglog(data4[:,0],data4[:,2],'ks-',label='slope='+slope)
pl.title('h versus norm2 error')
pl.xlabel('h')
pl.ylabel('norm2 error')
pl.legend()
pl.show()

data5=np.loadtxt('h_Itr.dat')
pl.plot(data5[:,0]**-1,data5[:,1],'ks-',label='jacobi')
pl.plot(data5[:,0]**-1,data5[:,2],'ko-',label='gaussSiedel')
pl.plot(data5[:,0]**-1,data5[:,3],'kx-',label='gaussSiedelRelaxed')
pl.title('h versus iterations')
pl.xlabel('h')
pl.ylabel('iterations')
pl.xlim()
pl.legend()
pl.show()


