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
outfile=open("datain.bc",'wb')

"""
#### Sequence ####
7,
71,72,73
1,8,10,20,23
9,6,5,2,21,22,24,28
3,32,33,34,35,19
"""

bctype1 = [71,72,73]
bctype2 = [1,8,10,20,23]
bctype3 = [9,6,5,2,21,22,24,28]
bctype4 = [3,32,33,34,35,19]

bctypeall = bctype1 +  bctype2 + bctype3 + bctype4
# [9,6,5,8,10,20,11,2,15,17,12,13,43,3,31,31,32,33,34,35,19,23,4,28,22,56,57]

# write bc 7
with open('bcoutput7.datain', "rb") as infile:
    for strings in infile:
        outfile.write(strings)
    outfile.write("\n")


# write separate bc
for bctype in bctypeall:
  for f in files:
    with open(f, "rb") as infile:
        for strings in infile:
            num = map(int, re.findall(r'\d+', strings))
            if len(num) != 0:
                if num[1] == bctype:
                    outfile.write(strings)
  outfile.write("\n")


outfile.write("&bcdef bcdir='end'/ \n")
outfile.write("&bcwake wbcdir='end'/ \n")
outfile.write("&spec_output optype = 'end'/ \n")

outfile.close()

#==============================================================================
# Boundary conditions:
# Note: Boundary conditions
# 1      zero gradient
# 2      supersonic inflow
# 3      no slip adiabatic wall boundary
# 4      zero gradient with w = 0
# 5      subsonic outflow, fixed static pressure (poutlet in datain)
# 6      subsonic inflow, fixed rho, u, v, w at inlet
# 7      inner boundary for mpi
# 8      symmetry boundary
# 9      subsonic inlet BC with fixed total pressure and temperature
#        (Note: direction of flow normal to face is not enforced and outflow is possible)
# 10     periodic boundary condtion
# 19     isothermal wall, zero-gradient pressure
# 21     subsonic inflow for rotor (applied over plane i-k)
# 22     subsonic outflow (fixed static pressure at freestream, applied over plane i-k)
# 23     periodical boundary for o mesh
# 24     Rimann subsonic inflow BC for rotor (applied over plane i-k)
# 28     subsonic outflow (curved static pressure profile at outlet)
# 31     partial-slip wall boundary in rotation coordinates
#        (if normal to wall in i direction- no treatment at all)
#        (if normal to wall in j direction- bctype 3 treatment)
#        (if normal to wall in k direction- wall functions treatment)
# 
# 32     moving wall boundary
# 33     dpdn = 0 wall boundary in rotation coordinates
# 34     dpdn = 0, dtdn = 0, psi changes wall boundary in rotation coordinates
# 35     dpdn = rv^2/r, dtdn = 0, wall boundary in rotation coordinates
# 70     Actuator boundary condition (only works if i direction is perpendicular to actuator line of actuator plane made by i-j)
# 71     rotating periodic boundary condition
# 72     Mixing plane boundary condition (Note: Normal to the mixing plane should be in j direction)
# 73     Periodic boundary condition
# 92     subsonic inlet BC with fixed total pressure and temperature
#        (Note: direction of flow normal to face is enforced)
# 95     special case: specify u=1, v=w=0 (eg. lid cavity), j-upper boundary
# 100    special case: shock/boundary layer interaction, j-upper boundary
# ***
# 101-110   isothermal wall, please modify the corresponding temperature value
#           (nondimensional temperature)
#           in namelist &iso_t-------->iso_tw,
#           For example, if in &bcdef bctype=105 is used ,
#                        the value of iso_tw at ! 105 should change to the
#                        value you want, for example, 1.02, 12.5,...
# 
#==============================================================================

# Rule: #
#1 Solid wall boundary fixed (end)
#2 inner boundary (BC7 defines first)
#3 Inlet Outlet boundary comes before wall and after other BC
#4 



