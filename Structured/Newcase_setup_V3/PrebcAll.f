       program PrebcAll
       implicit none

c	this program is to define the whole boundary conditions.
c 	input:  pre_bc7_datain, bcdefine.datain, 	init.input
c 	output: datain_BC
c	first double check datain_BC7 to be correct

c *** the variables of boundary condition
      character(len=3)::  bcdir
      integer::
     $   block,   bctype,  start(3),  end(3),
     $  iblock,           istart(3), iend(3),   order(3)
      integer
     $   nb,bc_max,blen,ic(3),ilmax,jlmax,klmax,pide  !!!
      integer, parameter::
     $     fdatain = 10, fbcdef = 20,
     $     foutputwall = 30,foutput7 = 40
     $     , foutputbc = 50

      namelist /bcdef/ bcdir, block, bctype, start,  end,
     $                       iblock,        istart, iend,  order

      integer,dimension(:),allocatable::
     $  bcinlet,   inletblkn,  inletblkdir,   inletblkend,
     $  bcoutlet, outletblkn, outletblkdir,  outletblkend
      integer ::
     $ bcinletNum,bcoutletNum,
     $ bctypeget=1,bctypeNum,
     $ nn, bcdir1,bcdirend,
     $ ii,jj,kk,i,j,blk_bc_type,
     $ blk_start, blk_dir, minmax,fbcdef_small
 
      integer :: blk_bc_info(10000,4), bc_type_total(1000)=0
                 !!! colume 1: bctype
                 !!! colume 2: block number 
                 !!! colume 3: block direction (xie=1, eta=2,zta=3)
                 !!! colume 4: block surface (0=lower, 1=upper) 

c-----------------------------Read prebc7 result----------------------------------------
      blk_bc_info = 0
      open(fbcdef_small,file='bcdefine.datain.small') 
      open(fbcdef,file='bcdefine.datain')

      ii = 1
      jj = 1

      do
         read(fbcdef_small,*) blk_bc_type, bctypeNum
         if(blk_bc_type .eq. -1) exit
         read(fbcdef_small,*) blk_start, blk_dir, minmax
          do nn =1, bctypeNum
             blk_bc_info(ii,1) = blk_bc_type
             blk_bc_info(ii,2) = blk_start+nn-1
             blk_bc_info(ii,3) = blk_dir
             blk_bc_info(ii,4) = minmax
             write(fbcdef,*) (blk_bc_info(ii,j),j=1,4)
             print*, (blk_bc_info(ii,j),j=1,4) 
             ii = ii+1
          end do
      end do

      print*, "bcdfine written"
      close(fbcdef_small)
      close(fbcdef)

      open(fdatain, file='pre_bc7_datain')
      open(foutputwall,file='bcoutputwall.datain')
      open(foutputbc,file='bcoutputbc.datain')
      open(foutput7,file='bcoutput7.datain')

c1****************Check Status & Output Value****************
      bcdir='xie'
      do while (bcdir .ne. 'end')
        read(fdatain,nml=bcdef)    !!! read a bc namelist
        if (bctype.eq.3) then
          if (bcdir .eq. 'xie') then 
             bcdir1 = 1         
             bcdirend = start(1)
          end if
          if (bcdir .eq. 'eta') then
             bcdir1 = 2   
             bcdirend = start(2)       
          end if 
          if (bcdir .eq. 'zta') then
             bcdir1 = 3
             bcdirend = start(3)
          end if

          do i = 1,ii
             if(  block .eq. blk_bc_info(i,2)   .and. 
     $           bcdir1 .eq. blk_bc_info(i,3)    .and.
     $         bcdirend .eq. blk_bc_info(i,4)  ) then
                 bctype = blk_bc_info(i,1) 
                 write(foutputbc,993) bcdir,block,bctype,start(1),
     $               start(2),start(3), end(1),end(2),end(3)
             end if
          end do
          if (bctype .eq. 3) 
     $       write(foutputwall,993) bcdir, block, bctype, start(1),
     $               start(2),start(3), end(1),end(2),end(3)
         else
c3****************write a line*********************
            write(foutput7,998) bcdir,block,start(1),start(2),start(3),
     $                             end(1),end(2),end(3)

            write(foutput7,999) iblock,istart(1),istart(2),istart(3),
     $                             iend(1),iend(2),iend(3),
     $                             order(1),order(2),order(3)
         end if

      end do
      write(foutputwall,*)
      write(foutputbc,*)
      write(foutput7,*)

      close(fdatain)
      close(fbcdef)
      close(foutputwall)
      close(foutputbc)
      close(foutput7)


c-----------------------------Close prebc7 result----------------------------------------

993   format("&bcdef bcdir=","'",a3,"'",", block=",
     > i3,", bctype=",i3,", start=",
     > i3,",",i3,",",i3,", end=",i3,",",i3,",",i3,"/")

996   format("&bcdef bcdir='end'/")

998   format("&bcdef bcdir=","'",a3,"'",", block=",
     > i3,", bctype=7, start=",
     > i3,",",i3,",",i3,", end=",i3,",",i3,",",i3)

999   format(17x," ,iblock=",i3,",          istart=",
     >      i3,",",i3,",",i3,",iend=",i3,",",i3,",",i3,
     >        ", order=",i3,",",i3,",",i3,"/")
     
      end program PrebcAll

