#pip install h5py
#pip install netcdf4

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

#std=np.std(jetHW)
jetHW_rms=np.sqrt((np.sum(np.square(jetHW)))/(jetHW.size))
zeros=np.zeros(time.size)
ones=zeros+1.000000
rms=ones*jetHW_rms
mline=ones*np.mean(jetHW)
pl.plot(time,jetHW,'r')
pl.plot(time,mline,'g')
pl.plot(time,rms,'b')
pl.show()

import math as mt
from scipy.fftpack import fft
#tdata=np.arange(0,time.size+1,1)
tdata=np.arange(0,time.size,1)
ut=fft(jetHW)
uts=np.fft.fftshift(ut)
utl=np.log(uts)
k1=2.00*(mt.pi)/tdata.size
k2=np.arange(-(tdata.size)/2,(tdata.size)/2,1)
k=k1*k2
#knew=np.arange(-200,200,1)
#pl.plot(k,utl)
pl.plot(k[(k.size/2):(k.size-1)],utl[(utl.size/2):(utl.size-1)])
pl.show()

