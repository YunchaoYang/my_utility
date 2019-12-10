      program plot3d_rans
c *****************************************************************
c     This program translates the geometry data with PLOT3D Format
c     to RANS-Solver Format
c *****************************************************************
      integer imax,jmax,kmax,ndim
      integer ngrid,i,j,k,m,n,ii
      integer, dimension(:), allocatable:: idim,jdim,kdim
      double precision, allocatable, dimension(:,:,:,:):: x
      double precision rl,alpha,pi,x01,x02
      character*20 filename
      character*7 pref
      open(4,file='geo.grd',status='old',form='unformatted')
      open(7,file='mesh_chk.grd',form='formatted')
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
      read(4)ngrid
c
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid))
c
      write(*,*)'The total number of subzones is',ngrid
      read(4)(idim(n),jdim(n),kdim(n),n=1,ngrid)
c
c *** compute the max values of the dimensions
c
      imax = 0
      jmax = 0
      kmax = 0
      do n=1,ngrid
        write(*,*)n,idim(n),jdim(n),kdim(n)
        if (imax.lt.idim(n)) imax = idim(n)
        if (jmax.lt.jdim(n)) jmax = jdim(n)
        if (kmax.lt.kdim(n)) kmax = kdim(n)
      end do
c
      allocate (x(imax,jmax,kmax,3))
c
      write(*,*)'please input the reference length:'
      read(*,*)rl
      write(*,*)'please input the rotation angle:'
      read(*,*)alpha
      alpha=-alpha*pi/180.d0
c
      write(7,*)ngrid
      do n=1,ngrid
        write(7,*)idim(n),jdim(n),kdim(n)
      end do
c
      do n=1,ngrid
        read(4)((((x(i,j,k,ii),i=1,idim(n)),
     $               j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
        write(*,*)'The subzones ',n,' has been read'
c
c *** dimensionless the coordintes
c
        do k = 1, kdim(n)
          do j = 1, jdim(n)
            do i = 1, idim(n)
              do m=1,ndim
                x(i,j,k,m)=x(i,j,k,m)/rl
              end do
            end do
          end do
        end do
c
        do k = 1, kdim(n)
          do j = 1, jdim(n)
            do i = 1, idim(n)
              x01=x(i,j,k,1)*dcos(alpha)-x(i,j,k,2)*dsin(alpha)
              x02=x(i,j,k,1)*dsin(alpha)+x(i,j,k,2)*dcos(alpha)
              x(i,j,k,1)=x01
              x(i,j,k,2)=x02
            end do
          end do
        end do
c
c *** write mesh information of current block
c
        write(filename, '(a7,i3.3,".dat")') pref,n
        open(9, file=filename,form='unformatted')
        write(9) idim(n)-1,jdim(n)-1,kdim(n)-1
        do k = 1, kdim(n)
          do j = 1, jdim(n)
            do i = 1, idim(n)
              write(9) (x(i,j,k,m),m=1,ndim)
            end do
          end do
        end do
        close(9)
c
        write(7,*)((((x(i,j,k,ii),i=1,idim(n)),
     $               j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
      end do
      close(4)
      close(7)
      deallocate(idim, jdim, kdim, x) 
c
      stop
      end
