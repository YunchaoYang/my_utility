#! /share/opt/python/2.7.3/bin/python

### define the tuple of direction
## bcdefine tuple definition:  
   # (bctype,block-start,block-end,direction,cell-index)
   # bctype: = 6, subsonic inlet; 5,subsonic outlet;...
   # block-start: start block number for this bc group
   # block-end: end block number for this bc group
   # direction: =1 xie, =2 eta, =3 zta
   # cell-index, =1 beginning;  =max end;  

########################################################################
print "BC defined as following:"
print "bctype, blk, direction, cell_index"
#############################################################################

import subprocess
subprocess.call(['./PrebcAll.o']) 

#############################################################################
from array import *
import re

files = ['bcoutputbc.datain','bcoutputwall.datain'] 
bctype1 = [9,6,5,8,10,20,11,2,15,17,12,13,43,3,31,31,32,33,34,35,19,23,4,28,22,56,57]
outfile=open("datain.bc",'wb')

# write separate bc
for bctype in bctype1:
  for f in files:
    with open(f, "rb") as infile:
        for strings in infile:
            num = map(int, re.findall(r'\d+', strings))
            if len(num) != 0:
                if num[1] == bctype:
                    outfile.write(strings)
  outfile.write("\n")

# write bc 7
with open('bcoutput7.datain', "rb") as infile:
    for strings in infile:
        outfile.write(strings)

outfile.write("&bcdef bcdir='end'/ \n")
outfile.write("&bcwake wbcdir='end'/ \n")
outfile.write("&spec_output optype = 'end'/ \n")

outfile.close()


