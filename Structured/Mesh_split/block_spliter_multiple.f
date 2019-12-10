      program block_splitter
c.. Alexis Lefebvre
c.. This program is implemented to split any block in i j and k direction into
c.. constant blocks of size imax,jmax,kmax.
c.. Input a plot3D unformated file, sigle block and specify how you want the
c.. block split.
      implicit none

      integer imax,jmax,kmax,imax2,jmax2,kmax2,isplit,jsplit,ksplit
      integer i,j,k,ii,n,n2,ngrid,ngrid2,split,i1,i2,i3,i4              ! ngrid = number of blocks
      integer, dimension(:), allocatable:: idim, jdim, kdim
      integer, dimension(:), allocatable:: idim2,jdim2,kdim2
      integer, dimension(:), allocatable:: is,ie,js,je,ks,ke
      double precision, allocatable, dimension(:,:,:,:,:):: x          ! mesh coordinates
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
      allocate (x(imax,jmax,kmax,3,ngrid))

c---------------------------------------------------------
c.. OUTPUT
c.. number of blocks in the new mesh
      isplit = (imax-1)/(imax2-1)
      jsplit = (jmax-1)/(jmax2-1)
      ksplit = (kmax-1)/(kmax2-1)
      ngrid2 = isplit*jsplit*ksplit !* ngrid ! Yang

c.. write output file in plot3d format, split in blocks
      allocate (is(ngrid2),ie(ngrid2),js(ngrid2),je(ngrid2),
     > ks(ngrid2),ke(ngrid2))

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

      allocate (idim2(ngrid*ngrid2),jdim2(ngrid*ngrid2),
     > kdim2(ngrid*ngrid2))

c.. define new block size
      do n=1,ngrid * ngrid2
         idim2(n)=imax2
         jdim2(n)=jmax2
         kdim2(n)=kmax2
      enddo

      write(5) ngrid*ngrid2
      write(5)(idim2(n),jdim2(n),kdim2(n),n=1,ngrid*ngrid2)

      write(*,*) 'ngrid= ',ngrid,' ngrid2 =',ngrid2
c---------------------------------------------------------
      do n = 1,ngrid
         read(4)((((x(i,j,k,ii,n),i=1,idim(n)),
     $        j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
         write(*,*) 'Zone ',n,' has been read'
c..output the splited mesh
      do n2=1,ngrid2
         write(5)((((x(i,j,k,ii,n),i=is(n2),ie(n2)),
     $        j=js(n2),je(n2)),k=ks(n2),ke(n2)),ii=1,3)
         write(*,*)'The subzone ',n2,' has been created'
      enddo
      enddo

      deallocate (idim,jdim,kdim)
      deallocate (idim2,jdim2,kdim2)


      endif  !!      if(split.eq.0) then




      end
      
