import numpy as np
a=np.array([1,2,3,4,-5,6,7,8,9,10])

for i in range(0,9):
  if a[i]<0:
      print 'possible flow reversal at cell',i
  else:
      print 'everything looks good'



