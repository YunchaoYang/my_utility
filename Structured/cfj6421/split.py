import re

inputname='clcdcm.dat'
filename = 'clcdcm-re.dat'

inputf=open(inputname,'r')
lines=inputf.readlines()


for cm in range(10,26,5):
   print cm
   filename='AoA'+str(cm)+'.plt'
   with open(filename,'w') as fiout:
      for line in lines:
         nums=re.split('\s+',line)
#         print nums
         if nums[1] == str(cm/100.0) :
            print nums[1]
            fiout.write(line)

inputf.close()
