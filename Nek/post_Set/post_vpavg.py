#! /usr/bin/python3.6
import re
import os
import subprocess
from subprocess import call
from glob import glob

#get current folder and folder name
Local = os.getcwd()
print(Local + "\n")
folder=os.path.split(os.getcwd())
print (folder[-1])

setfname='SettlingPart.dat'
with open(setfname, 'w') as output:
 logfiles = glob(os.getcwd()+'/*log*') 
 latest_file = max(logfiles, key=os.path.getctime)
 print(latest_file)
 try:
   filename=open(latest_file,'r')
   lines=filename.readlines()
   for line in lines:
     if line.startswith('Step'):
        line_items = line.split()
        ltime = line_items[3]
        output.write(line)
     if line.startswith('average '):
      if not ("NaN") in line:
       output.write(line)
 except IOError as e:
   print("Unable to open file") #Does not exist OR no read permissions

f1 =  open('SettlingPart1.dat', 'w')
f1.write('variables=t,Vx,Vy,Vz \n')
try:
   filename=open(setfname,'r')
   lines=filename.readlines()
   for i in range(len(lines)):
      line=lines[i]
      if line.startswith('Step'):
         line_items = line.split()
         timestep = line_items[1].replace(",","")
         ltime = line_items[3].replace(",","")        
         if lines[i+1].startswith('average '):
          newline = lines[i+1].replace('average particle velocity',str(ltime))
          f1.write(newline)
except IOError as e:
   print("Unable to open file") #Does not exist OR no read permissions

