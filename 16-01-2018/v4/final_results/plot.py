import pylab as pl
import numpy as np

kE=np.loadtxt("k_Vs_error.dat")
hE=np.loadtxt("h_Vs_error.dat")

slope, intercept = np.polyfit(np.log10(kE[:,0]), np.log10(kE[:,1]), 1)

pl.xlabel("deltaT(or k)")
pl.ylabel("Error (norm2)")
pl.title("Plot of deltaT(or k) Vs Error")
pl.plot(np.log10(kE[:,0]),np.log10(kE[:,1]),'r',label='slope='+str(slope))
pl.legend()
pl.savefig("k_Vs_error.png")
pl.show()



slope, intercept = np.polyfit(np.log10(hE[:,0]), np.log10(hE[:,1]), 1)

pl.xlabel("deltaX(or h)")
pl.ylabel("Error (norm2)")
pl.title("Plot of deltaX(or h) Vs Error")
pl.plot(np.log10(hE[:,0]),np.log10(hE[:,1]),'r',label='slope='+str(slope))
pl.legend()
pl.savefig("h_Vs_error.png")
pl.show()
