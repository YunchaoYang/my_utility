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
with open('clcdcm.dat', 'w') as output:

 for ang in range(02, 27, 04):
     # call(bsub_call % "run.job", shell=True)
    angdir='AoA'+str(ang).zfill(2)
    os.chdir(angdir)
    # print(os.getcwd()+'\n')
    folder=os.path.split(os.getcwd())
    print folder[-1]
    cmu = 4
# readlastline        
    filename=open('cdlt.his','rb')
    lines=filename.readlines()
    if lines:
       last_line = lines[-1]
       last_line =str(ang) + ' ' + str(cmu/100.0) + ' '+ last_line
       output.write(last_line)   
       
    for cmu in range(10,26,5):
        cmudir='Cmu.'+str(cmu).zfill(2)
#        os.mkdir(cmudir)
        os.chdir(cmudir)

#        print(os.getcwd()+'\n')
#        subprocess.call("ls")
        folder=os.path.split(os.getcwd())
        print folder[-1]

# readlastline        
        filename=open('cdlt.his','rb')
        lines=filename.readlines()
        if lines:
            last_line = lines[-1]
        last_line =str(ang) + ' ' + str(cmu/100.0) + ' '+ last_line

        output.write(last_line)

        os.chdir('..')

    os.chdir(Local)

os.chdir(Local)
print(subprocess.call("ls"))


inputname='clcdcm.dat'

inputf=open(inputname,'r')
lines=inputf.readlines()
inputf.close()

for cm in range(10,26,5):
   print cm
   filename='AoA'+str(cm)+'.plt'
   with open(filename,'w') as fiout:
      fiout.write('variables = AoA, Cmu, dstep, Cl, Cd, fxyz1, fxyz2,  tcm,trm,clp,clv, cdp,cdv \n')
      for line in lines:
         nums=re.split('\s+',line)
#         print nums
         if nums[1] == str(cm/100.0) :
            print nums[1]
            fiout.write(line)



#2 Good
#paths=glob('*/*')
#print paths
