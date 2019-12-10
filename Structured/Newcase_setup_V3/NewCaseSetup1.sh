#! /bin/bash

#1.  --> init.input, bin*.dat
echo "~~~~~~~~~~~~~~~FASIP Newcase BC setup~~~~~~~~~~~~~~~~"
echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
echo "Attention: make sure bcdefine.datain.small is correctly defined"

echo "Step 1 generate bin file and init.input"
./processing-1300.out <<EOF  
1
duct-cruise-80blk-41-31-41-sort.x
3
100
0
0.25
0
EOF
echo "Step 1 finished"
# >/dev/null
#2. Pre_bc7
echo "Step 2 find bc7"
./pre_bc7_2d3d.o <<EOF 
1d-8
3
EOF
echo "Step 2 finished, check the pre_bc7.err file "



#3. sort bc
echo "Step 3 sort  datain.bc"
 python Sort-Combine_datain_output.py
echo "Step 3 finished"