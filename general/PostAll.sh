#!/bin/bash

Local=$(pwd)
echo $Local


for ang in {02..46..04} ###
do
    
    cd $Local
    cd AoA$ang
    cp ../cfj005.mcr .
    cp ~/latestVersion/preplot .
    cp ../processing.out .

    ./processing.out <<EOF >/dev/null
    2
    0
    0
    0
    0
EOF
    for f in plot*.plt
    do
	./preplot $f Tran$f > /dev/null
    done
    rm plot* wall* *err *~ datain? *log tecplot.phy >/dev/null

    head="$!VarSet |MFBD|"
    newaddress="$!VarSet |MFBD| = '.'"
    sed -i "s/$head.*/ $newaddress /g" cfj005.mcr

    head="$!EXPORTSETUP EXPORTFNAME = 'sst005"
    cmcoef0="04"

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Mach.jpg'"
    tail="Mach.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef0-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Ps.jpg'"
    tail="Ps.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef0-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Pt.jpg'"
    tail="Pt.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef0-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr

    oldname="$!EXPORTSETUP EXPORTFNAME = 'streamlines.jpg'"
    tail="streamlines.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef0-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr
    
    ~/Tecplot/Install/bin/tec360 -b -p cfj005.mcr >/dev/null
    cp *jpg "$Local//Result"
    echo "finished"
    echo "============================================================"

   for cmcoef in {10..25..05}
   do 

    cd Cmu.$cmcoef

    cmcoef=$(echo $cmcoef)

    echo $cmcoef

    cp ../processing.out .
    cp ../../cfj005.mcr .
    cp ~/latestVersion/preplot .

    ./processing.out <<EOF >/dev/null
    2
    0
    0
    0
    0
EOF
    for f in plot*.plt
    do
	./preplot $f Tran$f >/dev/null 
    done
    rm plot* wall* *err *~ datain? *log tecplot.phy >/dev/null

    newaddress="$!VarSet |MFBD| = '.'"
    sed -i "s/$!VarSet |MFBD|.*/ $newaddress /g" cfj005.mcr

    head="$!EXPORTSETUP EXPORTFNAME = 'sst005"

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Mach.jpg'"
    tail="Mach.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Ps.jpg'"
    tail="Ps.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef-$tail"
    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr

    oldname="$!EXPORTSETUP EXPORTFNAME = 'Pt.jpg'"
    tail="Pt.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef-$tail"

    oldname="$!EXPORTSETUP EXPORTFNAME = 'streamlines.jpg'"
    tail="streamlines.jpg'"
    newname="$head-AoA$ang-Cmu.$cmcoef0-$tail"

    sed -i "s/$oldname.*/ $newname /g" cfj005.mcr

    ~/Tecplot/Install/bin/tec360 -b -p cfj005.mcr >/dev/null

    cp *jpg "$Local//Result"
    echo "finished"
    echo "============================================================"
    cd ../	
   done


#  next
    cd $Local
done
