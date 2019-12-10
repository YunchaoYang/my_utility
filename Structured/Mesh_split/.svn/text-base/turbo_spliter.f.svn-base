      program turbo_splitter
c.. Alexis Lefebvre
c.. This program is intended to help the user create a turbomachinery case.
c.. The user input a single blade plot3D unformated file, single block and a tip block if necessary.
c.. The software output any number of blade (by rotation) and any number of blocks as specify by user in mesh_check.grd
c.. The software also output the boundary conditions file in bc_output

      implicit none

      double precision Pi,IBPA,xinlet,xoutlet,dist,xcenter
      double precision xs1,xs2,xe1,xe2
      integer bl,nblade,nbladetot,rotate,tipbk,jltb,icenter
      integer ibc,ktip,bc1,bc2,n1,n2,nbk
      integer il,jl,kl                                               ! total mesh size
      integer ilb,jlb,klb                                            ! block size
      integer iltip,jltip,kltip                                      ! tip mesh size
      integer isplit,jsplit,ksplit                                   ! number of mesh split in i,j and k direction
      integer i,j,k,ii,n,ngrid,ngrid2,split,i1,i2,i3,i4              ! ngrid2 = number of blocks of the splitted mesh, ngrid =1
      integer, dimension(:), allocatable:: idim,jdim,kdim
      integer, dimension(:), allocatable:: is,ie,js,je,ks,ke
      double precision, allocatable, dimension(:,:,:,:,:):: x        ! mesh coordinates (i,j,k,coordinate x y or z,blade number)
      double precision, allocatable, dimension(:,:,:,:,:):: x2       ! mesh coordinates used when reconbining grid
      double precision, allocatable, dimension(:,:,:,:)  :: x3       ! mesh coordinates (for unspliting case)
      double precision, allocatable, dimension(:,:,:,:,:):: xtip     ! mesh coordinates for tip block
      character*50 filename,filename2


      nblade = 1

      write(*,*)
      write(*,*) 'Subroutine turbo_splitter'
      write(*,*) 'input file should be a plot3D unformated data'
      write(*,*)
c      write(*,*) 'Split mesh or unsplit mesh ? 0:split, 1:unsplit'
c      read (*,*) split
      split = 0
      write(*,*) 'Enter blade mesh file name '
      read (*,*) filename
      write(*,*)
      if(split.eq.0) then
         write(*,*) 'Enter splitted block size ilb,jlb,klb '
         read (*,*) ilb,jlb,klb
         write(*,*)
         write(*,*) 'Create multiple blades by rotation ?  0:no, 1:yes'
         read (*,*) rotate
         write(*,*)
         if (rotate.eq.1) then
            write(*,*) 'Enter the total number of blade in a full row
     >and number of blades in the mesh ?'
            write(*,*) 'The first number is only used to calculate the 
     >inter blade phase angle'
            read (*,*) nbladetot, nblade
         write(*,*)
         endif

         write(*,*)   'Does this case have tip block ?      0:no, 1:yes'
         read (*,*) tipbk
         write(*,*)
         if(tipbk.eq.1) then
            write(*,*) 'Enter tip block mesh file name '
            read (*,*) filename2
            write(*,*)
         endif

         write(*,*)   'Automatic boundary condition ?       0:no, 1:yes'        
         read (*,*) ibc
         write(*,*)
         if(ibc.eq.1) then                                       
           write(*,*) 'Do you have inlet boudary condition ? 
     >0:no, 1:yes'
           read(*,*) bc1
           write(*,*)
           write(*,*) 'Do you have outlet boudary condition ?
     >0:no, 1:yes' 
           read(*,*) bc2
           write(*,*)
          endif  !!  if(ibc.eq.1)
       endif     !!  if(split.eq.0)


      if(split.eq.1) then
         write(*,*) 'Enter original block size il,jl,kl '
         read (*,*) il,jl,kl  
         write(*,*)
      endif


      open ( unit=5, file='mesh_check.grd',form='unformatted' )
      open ( unit=4, file=filename, form='unformatted')
      if(tipbk.eq.1) open (unit=8, file=filename2, form='unformatted')



      if(split.eq.0) then
c--------------------------------------------------------
c.. BC INPUT
      if(ibc.eq.1) then        
        if (tipbk.eq.1) then
        write(*,*) 'Enter tip block k-index '
        write(*,*) 'This is the k index at which the blade stops and the
     > tip region starts'        
        read(*,*) ktip                                     ! tip block k-index
        write(*,*)
        endif
        open(1,file='bc.output')
      endif 


c--------------------------------------------------------
c.. MESH INPUT
c.. read input blade mesh file in plot3d format
      read(4) ngrid                                         ! read number of blocks (should be one here)
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid))

      read(4)(il,jl,kl,n=1,ngrid)                           ! read blocks dimension

        write(*,*)'Original block size il,jl,kl: '
        write(*,*) il,jl,kl

      allocate (x(il,jl,kl,3,nblade))

      do n=1,ngrid
         read(4)((((x(i,j,k,ii,1),i=1,il),j=1,jl),k=1,kl),ii=1,3)
      enddo

      deallocate (idim,jdim,kdim)


c--------------------------------------------------------
c.. MESH TIP INPUT
c.. read input tip mesh file in plot3d format
      if(tipbk.eq.1) then
      read(8) ngrid                                         ! read number of blocks (should be one here)
      read(8)(iltip,jltip,kltip,n=1,ngrid)                  ! read blocks dimension

      write(*,*)'Original tip block size iltip,jltip,kltip: '
      write(*,*) iltip,jltip,kltip

      allocate (xtip(iltip,jltip,kltip,3,nblade))

      do n=1,ngrid
         read(8)((((xtip(i,j,k,ii,1),i=1,iltip),j=1,jltip),
     >                               k=1,kltip),ii=1,3)
      enddo
      endif   !! if(tipbk.eq.1)


c---------------------------------------------------------
c.. OUTPUT
c.. number of blocks in the new mesh
      isplit = (il-1)/(ilb-1)
      jsplit = (jl-1)/(jlb-1)
      ksplit = (kl-1)/(klb-1)
      ngrid2 = isplit*jsplit*ksplit

c.. write output file in plot3d format, split in blocks
      allocate (is(ngrid2),ie(ngrid2),js(ngrid2),je(ngrid2),
     > ks(ngrid2),ke(ngrid2))


c.. define coefficients used to split one block into smaller block
c.. user can modify the order of the loops to order the blocks differently
      n = 1
      do k=1,ksplit
         do j=1,jsplit
            do i=1,isplit
               is(n)= (i-1)*(ilb-1) + 1
               ie(n)=  i*(ilb-1)    + 1
               js(n)= (j-1)*(jlb-1) + 1
               je(n)=  j*(jlb-1)    + 1
               ks(n)= (k-1)*(klb-1) + 1
               ke(n)=  k*(klb-1)    + 1    
               n = n+1
            enddo
         enddo
      enddo

c.. starting mesh rotation
      if(rotate.eq.1) then
         Pi = 2*ACOS(0.0)
         IBPA = 2*Pi/nbladetot
         write(*,*) 'IBPA =',IBPA*180/Pi,'deg'
 
         do bl=2,nblade        
         do i=1,il
         do j=1,jl
         do k=1,kl
           x(i,j,k,1,bl)=x(i,j,k,1,1)
           x(i,j,k,2,bl)=dcos((bl-1)*IBPA)*x(i,j,k,2,1)+
     >                   dsin((bl-1)*IBPA)*x(i,j,k,3,1)      
           x(i,j,k,3,bl)=dcos((bl-1)*IBPA)*x(i,j,k,3,1)-
     >                   dsin((bl-1)*IBPA)*x(i,j,k,2,1)
         enddo
         enddo
         enddo
         enddo

      if(tipbk.eq.1) then
         do bl=2,nblade        
         do i=1,iltip
         do j=1,jltip
         do k=1,kltip
           xtip(i,j,k,1,bl)=xtip(i,j,k,1,1)
           xtip(i,j,k,2,bl)=dcos((bl-1)*IBPA)*xtip(i,j,k,2,1)+
     >                      dsin((bl-1)*IBPA)*xtip(i,j,k,3,1)      
           xtip(i,j,k,3,bl)=dcos((bl-1)*IBPA)*xtip(i,j,k,3,1)-
     >                      dsin((bl-1)*IBPA)*xtip(i,j,k,2,1)
         enddo
         enddo
         enddo
         enddo         
      endif  !! if(tipbk.eq.1) then
      endif  !! if(rotate.eq.1) then


c..output the splited mesh
      if(tipbk.eq.1) then

      jltb = (jltip+1)/2                                   ! tip block mesh size in j direction after split
      write(5) ngrid2*nblade+2*nblade                      ! total nb of blocks
      write(5)((ilb,jlb,klb,n=1,ngrid2),                   ! block size
     $          (iltip,jltb,kltip,n=1,2),ii=1,nblade)
      do bl=1,nblade
         do n=1,ngrid2
            write(5)((((x(i,j,k,ii,bl),i=is(n),ie(n)),
     $           j=js(n),je(n)),k=ks(n),ke(n)),ii=1,3)
            if(n.eq.ngrid2) then
            write(5)((((xtip(i,j,k,ii,bl),i=1,iltip),
     $           j=1,jltb),k=1,kltip),ii=1,3)               ! tip block automatically splited in the middle of j direction   
            write(5)((((xtip(i,j,k,ii,bl),i=1,iltip),
     $           j=jltb,jltip),k=1,kltip),ii=1,3)
            endif
         enddo
      enddo

      else

      write(5) ngrid2*nblade                             ! total nb of blocks
      write(5)(ilb,jlb,klb,n=1,ngrid2*nblade)            ! block size
      do bl=1,nblade
         do n=1,ngrid2
            write(5)((((x(i,j,k,ii,bl),i=is(n),ie(n)),
     $           j=js(n),je(n)),k=ks(n),ke(n)),ii=1,3)
         enddo
      enddo     
      endif  !!     if(tipbk.eq.1)
      close(5)

c---------------------------------------------------------
c.. OUTPUT WALL BC
      if (ibc.eq.1) then
         write(1,*)
         write(1,*) "!!! BLADE WALL BC"
         if (tipbk.eq.0) then
            do bl=1,nblade
               do n=1,ngrid2
                  nbk = n+(bl-1)*(ngrid2+2*tipbk)
                  write(1,100)"eta",nbk,31,1,1,1,
     >                 ie(1)-1,1,ke(1)-1      
               enddo
            enddo
         else     !! if (tipbk.eq.0)
            do bl=1,nblade
               do n=1,ngrid2
                  nbk = n+(bl-1)*(ngrid2+2*tipbk)
                  write(1,100)"eta",nbk,31,1,1,1,
     >                 ie(1)-1,1,ktip-1      
               enddo
                  write(1,*)
                  nbk = ngrid2+(bl-1)*(ngrid2+2*tipbk)
                  write(1,100)"zta",nbk+1,31,                 ! write tip block BC
     >                 1,1,kltip-1,iltip-1,jltb-1,kltip-1 
                  write(1,100)"zta",nbk+2,31,
     >                 1,1,kltip-1,iltip-1,jltb-1,kltip-1 
                  write(1,100)"zta",nbk+1,31,
     >                 1,1,1,iltip-1,jltb-1,1 
                  write(1,100)"zta",nbk+2,31,
     >                 1,1,1,iltip-1,jltb-1,1 

                  write(1,*)
            enddo
         endif   !!  if (tipbk.eq.0)

         write(1,*)
         write(1,*)
         write(1,*) "!!! HUB WALL BC"      
         do bl=1,nblade
            do n=1,ngrid2
               nbk = n+(bl-1)*(ngrid2+2*tipbk)
               write(1,100)"zta",nbk,31,1,1,1,
     >              ie(1)-1,je(1)-1,1      
            enddo
         enddo
         write(1,*)
         write(1,*)
         write(1,*) "!!! CASING WALL BC"      
         do bl=1,nblade
            do n=1,ngrid2
               nbk = n+(bl-1)*(ngrid2+2*tipbk)
               write(1,100)"zta",nbk,31,1,1,ke(1)-1,
     >              ie(1)-1,je(1)-1,ke(1)-1      
            enddo
         enddo



c---------------------------------------------------------
c.. FIND INLET AND OUTLET PLAN   
         icenter = ie(1)-int(ie(1)/2)                        ! variable initialization
         xinlet  =  x(icenter,je(1),1,1,1)
         xoutlet =  x(icenter,je(1),1,1,1)
         do bl=1,1                                           ! 1 blade is enough to find the inlet and outlet plan
            do n=1,ngrid2 
               icenter = ie(n)-int(ie(1)/2)
               xcenter = x(icenter,je(n),1,1,bl)
               if (xinlet .gt.xcenter) xinlet  = xcenter
               if (xoutlet.lt.xcenter) xoutlet = xcenter
            enddo
         enddo
         
         write(*,*) "Inlet plan and outlet plan located at :"
         write(*,*) "xinlet =",xinlet,", xoutlet =",xoutlet
         write(*,*)



c---------------------------------------------------------
c.. WRITE INLET AND OUTLET BC   

         write(1,*)
         write(1,*)
         write(1,*) "!!! INLET BC" 
         do bl=1,nblade
            do n=1,ngrid2 
               icenter = ie(n)-int(ie(1)/2)
               dist = dabs(x(icenter,je(n),1,1,bl)-xinlet)  ! distance calculated at k=1 (hub),  j=jmax and i=imax/2 (i=1 would bring issues)
               if(dabs(dist).lt.1d-4) then        
               if(bc1.eq.1) then
               nbk = n+(bl-1)*(ngrid2+2*tipbk)
               write(1,100)"eta",nbk,24,1,je(1)-1,1,
     >              ie(1)-1,je(1)-1,ke(1)-1
               else
               write(1,100)"eta",nbk,999,1,je(1)-1,1,
     >              ie(1)-1,je(1)-1,ke(1)-1                
               endif
               endif
            enddo
         enddo 


         write(1,*)
         write(1,*)
         write(1,*) "!!! OUTLET BC" 
         do bl=1,nblade
            do n=1,ngrid2 
               icenter = ie(n)-int(ie(1)/2)
               dist = dabs(x(icenter,je(n),1,1,bl)-xoutlet)  ! distance calculated at k=1 (hub),  j=jmax and i=imax/2 (i=1 would bring issues)
               if(dabs(dist).lt.1d-4) then       
               if(bc2.eq.1) then
               nbk = n+(bl-1)*(ngrid2+2*tipbk)
               write(1,100)"eta",nbk,22,1,je(1)-1,1,
     >              ie(1)-1,je(1)-1,ke(1)-1
               else
               write(1,100)"eta",nbk,999,1,je(1)-1,1,
     >              ie(1)-1,je(1)-1,ke(1)-1

               endif
               endif
            enddo
         enddo 


c---------------------------------------------------------
c.. OUTPUT PERIODIC DISK BC
         write(1,*)
         write(1,*)
         write(1,*) "!!! PERIODIC BC" 
         do n1=1,int(ngrid2/2)                                 ! first half of the blocks
            xs1 = x(is(n1),je(n1),1,1,1)
            xe1 = x(ie(n1),je(n1),1,1,1)
           if(dabs(xe1-xs1).gt.1d-4) then                      ! condition true if this it is not an inlet or outlet block
              do n2=int(ngrid2/2)+1,ngrid2                     ! second half of the blocks
                 xs2 = x(is(n2),je(n2),1,1,1)
                 xe2 = x(ie(n2),je(n2),1,1,1)
                 if(dabs(xe2-xs2).gt.1d-4) then                ! condition true if this it is not an inlet or outlet block               
                 if(dabs(xs1-xe2).lt.1d-4) then
                  nbk = n2+(nblade-1)*ngrid2+tipbk*(2*(nblade-1))
                  write(1,101) "eta",n1,73,1,je(1)-1,1,
     >                         ie(1)-1,je(1)-1,ke(1)-1
                  write(1,102) nbk,ie(1)-1,je(1)-1,1,
     >                        1,je(1)-1,ke(1)-1
                 endif
                 endif
               enddo
            endif
         enddo


      endif   !!  if (ibc.eq.1) 
      deallocate (x)
      write(*,*) "Output file ready in mesh_check.grd"
      write(*,*) "Plot3D unformated data"

      endif   !!  if(split.eq.0) 



 100  format("&bcdef bcdir=","'",a3,"'",", block=",
     > i3,", bctype=",i3,", start=",i3,",",i3,",",i3,
     > ", end=",i3,",",i3,",",i3,"/")

 101  format("&bcdef bcdir=","'",a3,"'",", block=",
     > i3,", bctype=",i3,", start=",i3,",",i3,",",i3,
     > ", end=",i3,",",i3,",",i3)
 102  format(17x," ,iblock=",i3,",",12x,"istart=",i3,",",i3,",",i3,
     >      ",iend=",i3,",",i3,",",i3,", order=-1,2,3/")











calex
c     Use the block_spliter version of the code to merge code, this one is not validated

      if(split.eq.1) then     
c--------------------------------------------------------
c.. INPUT
c.. read the splited mesh 
      read(4) ngrid                                         ! read number of blocks
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid))
      read(4)(idim(n),jdim(n),kdim(n),n=1,ngrid)            ! read blocks dimension

      ilb = 0                                             ! find the largest block dimension (nor really used here has all the block are supposed to be the same size)
      jlb = 0
      klb = 0
      write(*,*)'Splited block size ilb,jlb,klb: '  
      do n=1,ngrid   
      write(*,*) idim(n),jdim(n),kdim(n)
      if (ilb.lt.idim(n)) ilb = idim(n)
      if (jlb.lt.jdim(n)) jlb = jdim(n)
      if (klb.lt.kdim(n)) klb = kdim(n)
      enddo
      allocate (x2(ilb,jlb,klb,3,ngrid))
      allocate (x3(il,jl,kl,3))

      do n=1,ngrid                                          ! read splitted block mesh coordinates
         read(4)((((x2(i,j,k,ii,n),i=1,idim(n)),
     $        j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
         write(*,*) 'The subzone ',n,' has been read'
      enddo


c.. number of splits
      isplit = (il-1)/(ilb-1)
      jsplit = (jl-1)/(jlb-1)
      ksplit = (kl-1)/(klb-1)
      ngrid2 = isplit*jsplit*ksplit
      print*,'number of split in i,j and k direction :'
      print*,isplit,jsplit,ksplit
      


c.. join blocks in i, j and k direction
c.. write all the data without writting the ilb domain (so that those domain are not repeated)
      do n=1,ngrid2                                                    ! loop on the number of blocks of splitted mesh
         do k=1,klb
            do j=1,jlb
               do i=1,ilb-1                                            ! -1 to not repeat the last domain
                  i1 = i+(n-1)*(idim(n)-1)                             ! i index for the unsplitted mesh
                  i2 = i+(n-1-isplit)*(idim(n)-1)
                  i3 = i+(n-1-2*isplit)*(idim(n)-1)
                  i4 = i+(n-1-3*isplit)*(idim(n)-1)
                  do ii=1,3

                     if (n.lt.(isplit+1)) then
                        x3(i1,j,k,ii)         = x2(i,j,k,ii,n)          ! transform back the data into the no-split mesh coordinates
                     endif
                     
                     if (n.gt.isplit.and.n.lt.(2*isplit+1)) then       ! join for j split (only one split in j direction)
                        x3(i2,j+jlb-1,k,ii) = x2(i,j,k,ii,n)
                     endif

                     if (n.gt.2*isplit.and.n.lt.(3*isplit+1)) then     ! join for k (only one split in k direction)
                        x3(i3,j,k+klb-1,ii) = x2(i,j,k,ii,n)
                     endif
                     if (n.gt.3*isplit) then
                        x3(i4,j+jlb-1,k+klb-1,ii) = x2(i,j,k,ii,n) 
                     endif

                  enddo
               enddo
            enddo
         enddo
      enddo


c.. write the last domain
      do k=1,klb
         do j=1,jlb
            do ii=1,3
               x3(il,j,k,ii)= x2(ilb,j,k,ii,isplit)
               if(jsplit.eq.2) then
               x3(il,j+jlb-1,k,ii)= x2(ilb,j,k,ii,2*isplit)
               endif
               if(ksplit.eq.2) then
              x3(il,j,k+klb-1,ii)= x2(ilb,j,k,ii,3*isplit)
              x3(il,j+jlb-1,k+klb-1,ii)= x2(ilb,j,k,ii,4*isplit)
               endif                
            enddo
         enddo
      enddo



      deallocate (x2)
c..output the no-split mesh
      write(5) 1
      write(5)(il,jl,kl,n=1,1)
      write(5)((((x3(i,j,k,ii),i=1,il),
     $     j=1,jl),k=1,kl),ii=1,3)
      write(*,*) 
      write(*,*) 'Unsplitted mesh in mesh_check.grd'



      endif  !!      if(split.eq.0) then      
      end
      
