#! /share/opt/python/2.7.3/bin/python

import re
import os
import subprocess
from subprocess import call
from glob import glob

bsub_call = "bsub < "

#get current folder and folder name
Local = os.getcwd()
print(Local + "\n")
folder=os.path.split(os.getcwd())
print folder[-1]

with open('jet_all.dat', 'w') as output:

 for ang in range(02, 50, 04):
#    call(bsub_call % "run.job", shell=True)
    angdir='AoA'+str(ang).zfill(2)
    os.chdir(angdir)
   #print(os.getcwd()+'\n')
    folder=os.path.split(os.getcwd())
    print folder[-1]
    cmu = 4

# readlastline            
    try:
     filename=open('jet.his','rb')
     lines=filename.readlines()
     if lines:
       last_line = lines[-1]
       last_line =str(ang) + ' ' + str(cmu/100.0) + ' '+ last_line
       output.write(last_line)   
    except IOError as e:
       print "Unable to open file" #Does not exist OR no read permissions

    for cmu in [10,15,20,25]:
       cmudir='Cmu.'+str(cmu).zfill(2)
#        os.mkdir(cmudir)
       os.chdir(cmudir)

#        print(os.getcwd()+'\n')
#        subprocess.call("ls")
       folder=os.path.split(os.getcwd())
       print folder[-1]

# readlastline        
       try:
          filename=open('jet.his','rb')
          lines=filename.readlines()
          if lines:
             last_line = lines[-1]
             last_line =str(ang) + ' ' + str(cmu/100.0) + ' '+ last_line            
             output.write(last_line)
       except IOError as e:
          print "Unable to open file" #Does not exist OR no read permissions

       os.chdir('..')

    os.chdir(Local)

inputname='jet_all.dat'

inputf=open(inputname,'r')
lines=inputf.readlines()
inputf.close()

for cm in [04]+range(10,26,5):
   filename='Cmujet'+str(cm).zfill(2)+'.plt'
   print filename
   with open(filename,'w') as fiout:
      fiout.write('variables = AoA, Cmu, Iter, minj, msuc, cmu, pc, vj, Pt_inj,  Ps_suc, Pti, Pts, Tti \n')
      for line in lines:
         nums=re.split('\s+',line)
#         print nums
         if nums[1] == str(cm/100.0) :
           # print nums[1]
            fiout.write(line)

os.chdir(Local)
print(subprocess.call("ls"))

#2 Good
#paths=glob('*/*')
#print paths
