#!/bin/bash

Local=$(pwd)
echo $Local


for ang in {02..46..04} ###
do
    
    cd $Local
    cd AoA$ang

    cp ../cfj005-suction.mcr .

    head="$!EXPORTSETUP EXPORTFNAME = 'sst005"
    cmcoef="04"

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Mach-suction.jpg'"
    tail="Mach-suction.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005-suction.mcr
  
    ~/Tecplot/Install/bin/tec360 -b -p cfj005-suction.mcr >/dev/null
    cp *jpg "$Local//Result"

    echo "finished"
    echo "============================================================"

   for cmcoef in {10..25..05}
   do 

    cd Cmu.$cmcoef

    cmcoef=$(echo $cmcoef)

    echo $cmcoef
    cp ../../cfj005-suction.mcr .

    head="$!EXPORTSETUP EXPORTFNAME = 'sst005"

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Mach-suction.jpg'"
    tail="Mach-suction.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005-suction.mcr
  
    ~/Tecplot/Install/bin/tec360 -b -p cfj005-suction.mcr >/dev/null
    cp *jpg "$Local//Result"

    echo "finished"
    echo "============================================================"


    cd ../	
   done


#  next
    cd $Local
done
