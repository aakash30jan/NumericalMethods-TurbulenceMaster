import numpy as np
data1=np.loadtxt('thomas_v6_errorData1.dat')
data2=np.loadtxt('thomas_v6_errorData2.dat')
import pylab as pl

slope, intercept = np.polyfit(np.log10(data1[:,0]), np.log10(data1[:,1]), 1)
pl.xlabel('(n)Number of Discritization Points (log10)')
pl.ylabel('Mean Error (log10)')
pl.title('Error in computation of tridiagonal matrix (n=4 to 30660)')
pl.plot(np.log10(data1[:,0]),np.log10(data1[:,1]),'r',label='slope='+str(slope))
pl.legend()
pl.savefig('data1_NPlog.png')
pl.show()

pl.xlabel('(n)Number of Discritization Points')
pl.ylabel('Mean Error')
pl.title('Error in computation of tridiagonal matrix (n=4 to 30660)')
pl.loglog(data1[:,0],data1[:,1],'g',label='slope='+str(slope))
pl.legend()
pl.savefig('data1_PLlog.png')
pl.show()


slope, intercept = np.polyfit(np.log10(data2[:,0]), np.log10(data2[:,1]), 1)
pl.xlabel('(n)Number of Discritization Points (log10)')
pl.ylabel('Mean Error (log10)')
pl.title('Error in  computation of tridiagonal matrix (n=4 to 10000 with step of 10)')
pl.plot(np.log10(data2[:,0]),np.log10(data2[:,1]),'r',label='slope='+str(slope))
pl.legend()
pl.savefig('data2_NPlog.png')
pl.show()

slope, intercept = np.polyfit(np.log10(data2[:,0]), np.log10(data2[:,1]), 1)
pl.xlabel('(n)Number of Discritization Points')
pl.ylabel('Mean Error')
pl.title('Error in computation of tridiagonal matrix (n=4 to 10000 with step of 10)')
pl.loglog(data2[:,0],data2[:,1],'g',label='slope='+str(slope))
pl.legend()
pl.savefig('data2_PLlog.png')
pl.show()


