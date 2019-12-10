      program rans_plot3d
c *****************************************************************
c     This program translates the geometry data with PLOT3D Format
c     to RANS-Solver Format
c *****************************************************************


      implicit none
      
      integer choice
      
      print*,'Extracting geometry from rstore or bin-file'
      print*,'to plot3d ASCII file in mesh_chk.grd.'
      print*
      print*,'Options'
      print*,'<1> Extract from bin-files'
      print*,'<2> Extract from rstore-files'
      read(*,*)choice

      select case(choice)
      case(1)
         call binreader
      case(2)
         call rstore_grd
      end select

      stop 
      end

*     ----------------------

      subroutine binreader

      implicit none

      integer imax,jmax,kmax,ndim
      integer ngrid,i,j,k,m,n,ii,id,jd,kd
      integer, dimension(:), allocatable:: idim,jdim,kdim,bdim
      double precision, allocatable, dimension(:,:,:,:):: x
      double precision rl,alpha,pi,x01,x02
      character*20 filename
      character*7 pref


c *** constant
      pi=4.d0*atan(1.)
c
      write(*,*)'please input the dimension (2/3):'
      read(*,*) ndim
      if (ndim.eq.2) then
        pref="bin_2d_"
      else
        pref="bin_3d_"
      end if
      write(*,*)'how many subzones in mesh?'
      read(*,*)ngrid
c
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid),bdim(ndim))

c
c *** compute the max values of the dimensions
c
      imax = 0
      jmax = 0
      kmax = 0
      do n=1,ngrid
        write(filename, '(a7,i3.3,".dat")') pref,n
        print*,filename
        open(4, file=filename,form='unformatted')
        read(4) (bdim(ii),ii=1,ndim)
        idim(n) = bdim(1)+1
        jdim(n) = bdim(2)+1
        if(ndim.lt.3) bdim(3) = 0
        kdim(n) = bdim(3)+1
        close(4)

        write(*,*)n,idim(n),jdim(n),kdim(n)
        if (imax.lt.idim(n)) imax = idim(n)
        if (jmax.lt.jdim(n)) jmax = jdim(n)
        if (kmax.lt.kdim(n)) kmax = kdim(n)
      enddo
c
      allocate (x(imax,jmax,kmax,3))
c
      open(7,file='mesh_chk.grd',form='formatted')
c
c
      write(7,*)ngrid
      do n=1,ngrid
        write(7,*)idim(n),jdim(n),kdim(n)
      end do
c
      do n=1,ngrid
        write(filename, '(a7,i3.3,".dat")') pref,n
        open(9, file=filename,form='unformatted')
        read(9) 
        do k = 1, kdim(n)
          do j = 1, jdim(n)
            do i = 1, idim(n)
              read(9) (x(i,j,k,m),m=1,ndim)
            end do
          end do
        end do
        close(9)
c
        write(7,*)((((x(i,j,k,ii),i=1,idim(n)),
     $               j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
      end do

      close(9)
      close(7)
      deallocate(idim, jdim, kdim, x) 
c
      return
      end


c-----------------------------------------------------------------

      subroutine rstore_grd

      implicit real*8(a-h,o-z)

      character(len=20):: fiLe1,file2

      integer :: ii,nl,ntotal,istep

      integer :: xilow,xiupp,etlow,etupp,ztlow,ztupp,blen
      integer :: xlow,xupp,elow,eupp,zlow,zupp
      integer,dimension(:),allocatable:: iL,jL,kL,iLp,jLp,kLp
      double precision,dimension(:,:,:),allocatable :: x,y,z
      double precision,dimension(:,:,:,:),allocatable :: xx
      integer :: iuns,i2d


      print*,'Unsteady? <1>yes <0>no'
      read(*,*)iuns

      if (iuns.eq.1) then
         print*,'dstep?:'
         read(*,*)istep
      endif

      print*,'<2> for 2D mesh'
      print*,'<3> for 3D mesh'
      print*,'(choose 3D if not known)'
      read(*,*)i2d
      
      open(1,file='init.input')
      read(1,*)
      read(1,*)nl
      
      allocate(iL(nl),jL(nl),kL(nl),iLp(nl),jLp(nl),kLp(nl))

      imax = 0
      jmax = 0
      kmax = 0
      do ii=1,nl
         read(1,*)iL(ii),jL(ii),kL(ii)
         iLp(ii)=iL(ii)+1
         jLp(ii)=jL(ii)+1
         kLp(ii)=kL(ii)+1
         if (i2d.eq.2) kLp(ii) = 1
         if (imax.lt.iLp(ii)) imax = iLp(ii)
         if (jmax.lt.jLp(ii)) jmax = jLp(ii)
         if (kmax.lt.kLp(ii)) kmax = kLp(ii)
      enddo

      allocate(xx(imax,jmax,kmax,3))

      open(7,file='mesh_chk.grd',form='formatted')
c
      write(7,*)nl
      do ii=1,nl
        write(7,*)iLp(ii),jLp(ii),kLp(ii)
      end do

      do ii=1,nl
         if(iuns.eq.1) then
            if(istep.ge.1) 
     $           write(fiLe1, '("rstore",i3.3,"_",i7.7)') ii,istep
         else
            write(fiLe1, '("rstore",i3.3)') ii
         endif

         open(2,fiLe=fiLe1,form= 'unformatted')
         read(2) ntotal, time, blen
         read(2) ip, jp, kp

c  check dimension of each block
         if(ip.ne.iL(ii).or.jp.ne.jL(ii).or.kp.ne.kL(ii)) then
            write(*,*)'ip  ,jp  ,kp  =',ip,jp,kp
            write(*,*)'iL  ,jL  ,kL  =',iL(ii),jL(ii),kL(ii)
            print *, "dimension don't match in input and result fiLes"
            stop
         end if
      
         xilow=1-blen
         xiupp=il(ii)+blen
         etlow=1-blen
         etupp=jl(ii)+blen
         ztlow=1-blen
         ztupp=kl(ii)+blen
         
         allocate(x(xiLow+1:xiupp,etlow+1:etupp,ztlow+1:ztupp) 
     $        ,y(xiLow+1:xiupp,etlow+1:etupp,ztlow+1:ztupp) 
     $        ,z(xiLow+1:xiupp,etlow+1:etupp,ztlow+1:ztupp))
         

c read grid including blen area           
         do k = ztlow+1, ztupp
            do j = etlow+1, etupp
               do i = xiLow+1, xiupp
                  read(2) x(i,j,k), y(i,j,k), z(i,j,k)
               end do
            end do
         end do
         
         print*, '>>> read rstore done for BLOCK = ',ii
         close(2)
         
         do k = 1,klp(ii)
            do j = 1,jlp(ii)
               do i = 1,ilp(ii)
                  xx(i,j,k,1) = x(i,j,k)
                  xx(i,j,k,2) = y(i,j,k)
                  xx(i,j,k,3) = z(i,j,k)
               enddo
            enddo
         enddo
         
         write(7,*)((((xx(i,j,k,n),i=1,ilp(ii)),
     $        j=1,jlp(ii)),k=1,klp(ii)),n=1,3)

         deallocate(x,y,z)

      enddo

      close(7)
      deallocate(iL,jL,kL,iLp,jLp,kLp)

      return
      end
