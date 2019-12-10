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
a=(6,27,32,2,25) # Inlet
b=(6,57,58,2,25)

c=(5,25,26,2,25) # outlet
d=(5,59,64,2,25)
e=(5,77,80,2,25)

f=(10,1,94,3,1)  # bc10 periodic
g=(10,1,94,3,40)


h=(9,81,81,1,1)  # CFJ inlet
i=(5,94,94,1,40) # CFJ outlet

listbctuple = []
for tup in a,b,c,d,e,f,g,h,i:
  listbctuple.append(tup)

########################################################################

with open("bcdefine.datain",'wb') as bcdefineoutfile:
  for bctype, blk_start, blk_end, direction,cell_index in listbctuple:
#    print bctype, blk_start, blk_end, direction,cell_index
    blocks=blk_end-blk_start+1
    bcdefineoutfile.write(str(bctype)+','+str(blocks)+'\n')
    for blk in range(blk_start,blk_end+1):
      bcdefineoutfile.write(str(blk)+','+str(direction)+','+str(cell_index)+'\n')
      print str(blk), direction, cell_index
  bcdefineoutfile.write(str(-1)+','+str(0))
#############################################################################

import subprocess
subprocess.call(['./PrebcAll.o']) 

#############################################################################
from array import *
import re

files = ['bcoutputbc.datain','bcoutputwall.datain'] 
bctype1 = [9,6,5,10,3]
outfile=open("datain.bc",'wb')
# write separate bc
for bctype in bctype1:
  for f in files:
    with open(f, "rb") as infile:
        for strings in infile:
            num=map(int, re.findall(r'\d+', strings))
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
