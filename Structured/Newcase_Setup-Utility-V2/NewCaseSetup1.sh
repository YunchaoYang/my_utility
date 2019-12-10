#! /bin/bash

#1.  --> init.input, bin*.dat
./processing_init.input.out <<EOF >/dev/null
1
4million-94blk.x
3
1
30
0.25
0
EOF

#2. Pre_bc7
./pre_bc7.o <<EOF >/dev/null
1d-8
EOF


#3.Modify bcdefine.datain

#4.  --> bc
#./PrebcAll.sh

#cp datain.bc bin* init.input ../destination
#rm pre_bc7* bcoutput* mesh_check*

3 python Sort-Combine_datain_output.py
