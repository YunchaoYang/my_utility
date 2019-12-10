      program block_splitter
c.. Alexis Lefebvre
c.. This program is implemented to split any block in i j and k direction into
c.. constant blocks of size imax,jmax,kmax.
c.. Input a plot3D unformated file, sigle block and specify how you want the
c.. block split.
      implicit none

      integer imax,jmax,kmax,imax2,jmax2,kmax2,isplit,jsplit,ksplit
      integer i,j,k,ii,n,ngrid,ngrid2,split,i1,i2,i3,i4              ! ngrid = number of blocks
      integer, dimension(:), allocatable:: idim,jdim,kdim
      integer, dimension(:), allocatable:: is,ie,js,je,ks,ke
      double precision, allocatable, dimension(:,:,:,:):: x          ! mesh coordinates
      double precision, allocatable, dimension(:,:,:,:,:):: x2       ! mesh coordinates used when reconbining grid

      character*50 filename


c ############################################
c The unspliting program program allow for multiple split in i directin
c but only one plit in j and k direction. Modify the part
c 'loop on the number of blocks of splitted mesh' accordingly
c to your case if your case is different.
c #############################################

      write(*,*)
      write(*,*) 'Subroutine block_splitter'
      write(*,*) 'input file should be a plot3D unformated file'
      write(*,*)
      write(*,*) 'Split mesh or unsplit mesh ? 0:split, 1:unsplit'
      read (*,*) split
      write(*,*)
      write(*,*) 'Ente file name '
      read (*,*) filename
      if(split.eq.0) then
      write(*,*) 'Enter splitted block size imax,jmax,kmax '
      read (*,*) imax2,jmax2,kmax2
      endif
      if(split.eq.1) then
      write(*,*) 'Enter original block size imax,jmax,kmax '
      read (*,*) imax,jmax,kmax  
      endif



      open ( unit=5, file='mesh_check.grd',form='unformatted' )
      open ( unit=4, file=filename, form='unformatted')

      if(split.eq.0) then
c--------------------------------------------------------
c.. INPUT
c.. read input mesh file in plot3d format
      read(4) ngrid                                         ! read number of blocks (should be one here)
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid))

      read(4)(idim(n),jdim(n),kdim(n),n=1,ngrid)            ! read blocks dimension

      imax = 0
      jmax = 0
      kmax = 0
      do n=1,ngrid
        write(*,*)'Original block size imax.jmax.kmax: '
        write(*,*) idim(n),jdim(n),kdim(n)
        if (imax.lt.idim(n)) imax = idim(n)
        if (jmax.lt.jdim(n)) jmax = jdim(n)
        if (kmax.lt.kdim(n)) kmax = kdim(n)
      end do
      allocate (x(imax,jmax,kmax,3))

      do n=1,ngrid
         read(4)((((x(i,j,k,ii),i=1,idim(n)),
     $        j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
      enddo

      deallocate (idim,jdim,kdim)
c---------------------------------------------------------
c.. OUTPUT
c.. number of blocks in the new mesh
      isplit = (imax-1)/(imax2-1)
      jsplit = (jmax-1)/(jmax2-1)
      ksplit = (kmax-1)/(kmax2-1)
      ngrid2 = isplit*jsplit*ksplit

c.. write output file in plot3d format, split in blocks
      allocate (is(ngrid2),ie(ngrid2),js(ngrid2),je(ngrid2),
     > ks(ngrid2),ke(ngrid2))
      allocate (idim(ngrid2),jdim(ngrid2),kdim(ngrid2))

c.. define block size
      do n=1,ngrid2
         idim(n)=imax2
         jdim(n)=jmax2
         kdim(n)=kmax2
      enddo


c.. define coefficients used to split one block into smaller block
c.. user can modify the order of the loops to order the blocks differently
      n = 1
      do k=1,ksplit
         do j=1,jsplit
            do i=1,isplit
               is(n)= (i-1)*(imax2-1) + 1
               ie(n)=  i*(imax2-1)    + 1
               js(n)= (j-1)*(jmax2-1) + 1
               je(n)=  j*(jmax2-1)    + 1
               ks(n)= (k-1)*(kmax2-1) + 1
               ke(n)=  k*(kmax2-1)    + 1    
               n = n+1
            enddo
         enddo
      enddo

c..output the splited mesh
      write(5) ngrid2
      write(5)(idim(n),jdim(n),kdim(n),n=1,ngrid2)
      do n=1,ngrid2
         write(5)((((x(i,j,k,ii),i=is(n),ie(n)),
     $        j=js(n),je(n)),k=ks(n),ke(n)),ii=1,3)
         write(*,*)'The subzone ',n,' has been created'
      enddo
      endif  !!      if(split.eq.0) then









      if(split.eq.1) then     
c--------------------------------------------------------
c.. INPUT
c.. read the splited mesh 
      read(4) ngrid                                         ! read number of blocks
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid))
      read(4)(idim(n),jdim(n),kdim(n),n=1,ngrid)            ! read blocks dimension

      imax2 = 0                                             ! find the largest block dimension (nor really used here has all the block are supposed to be the same size)
      jmax2 = 0
      kmax2 = 0
      write(*,*)'Splited block size imax.jmax.kmax: '  
      do n=1,ngrid   
      write(*,*) idim(n),jdim(n),kdim(n)
      if (imax2.lt.idim(n)) imax2 = idim(n)
      if (jmax2.lt.jdim(n)) jmax2 = jdim(n)
      if (kmax2.lt.kdim(n)) kmax2 = kdim(n)
      enddo
      allocate (x2(imax2,jmax2,kmax2,3,ngrid))
      allocate (x(imax,jmax,kmax,3))

      do n=1,ngrid                                          ! read splitted block mesh coordinates
         read(4)((((x2(i,j,k,ii,n),i=1,idim(n)),
     $        j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
         write(*,*) 'The subzone ',n,' has been read'
      enddo


c.. number of splits
      isplit = (imax-1)/(imax2-1)
      jsplit = (jmax-1)/(jmax2-1)
      ksplit = (kmax-1)/(kmax2-1)
      ngrid2 = isplit*jsplit*ksplit
      print*,'number of split in i,j and k direction :'
      print*,isplit,jsplit,ksplit
      


c.. join blocks in i, j and k direction
c.. write all the data without writting the imax2 domain (so that those domain are not repeated)
      do n=1,ngrid2                                                    ! loop on the number of blocks of splitted mesh
         do k=1,kmax2
            do j=1,jmax2
               do i=1,imax2-1                                          ! -1 to not repeat the last domain
                  i1 = i+(n-1)*(idim(n)-1)                             ! i index for the unsplitted mesh
                  i2 = i+(n-1-isplit)*(idim(n)-1)
                  i3 = i+(n-1-2*isplit)*(idim(n)-1)
                  i4 = i+(n-1-3*isplit)*(idim(n)-1)
                  do ii=1,3

                     if (n.lt.(isplit+1)) then
                        x(i1,j,k,ii)         = x2(i,j,k,ii,n)          ! transform back the data into the no-split mesh coordinates
                     endif
                     
                     if (n.gt.isplit.and.n.lt.(2*isplit+1)) then       ! join for j split (only one split in j direction)
                        x(i2,j+jmax2-1,k,ii) = x2(i,j,k,ii,n)
                     endif

                     if (n.gt.2*isplit.and.n.lt.(3*isplit+1)) then     ! join for k (only one split in k direction)
                        x(i3,j,k+kmax2-1,ii) = x2(i,j,k,ii,n)
                     endif
                     if (n.gt.3*isplit) then
                        x(i4,j+jmax2-1,k+kmax2-1,ii) = x2(i,j,k,ii,n) 
                     endif

                  enddo
               enddo
            enddo
         enddo
      enddo


c.. write the last domain
      do k=1,kmax2
         do j=1,jmax2
            do ii=1,3
               x(imax,j,k,ii)= x2(imax2,j,k,ii,isplit)
               if(jsplit.eq.2) then
               x(imax,j+jmax2-1,k,ii)= x2(imax2,j,k,ii,2*isplit)
               endif
               if(ksplit.eq.2) then
               x(imax,j,k+kmax2-1,ii)= x2(imax2,j,k,ii,3*isplit)
               x(imax,j+jmax2-1,k+kmax2-1,ii)= x2(imax2,j,k,ii,4*isplit)
               endif                
            enddo
         enddo
      enddo



      deallocate (x2)
c..output the no-split mesh
      write(5) 1
      write(5)(imax,jmax,kmax,n=1,1)
      write(5)((((x(i,j,k,ii),i=1,imax),
     $     j=1,jmax),k=1,kmax),ii=1,3)
      write(*,*) 
      write(*,*) 'Unsplitted mesh in mesh_check.grd'



      endif  !!      if(split.eq.0) then      
      end
      
