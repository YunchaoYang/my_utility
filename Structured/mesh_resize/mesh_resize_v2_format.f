      program mesh_resize

c *****************************************************************
c     This program intends to conduct mesh refinement study
c     Input:  existing mesh
c     Output: 
c     options: 1 xie: double
c            : 2 xie: half
c              3 eta: double
c              4 eta: half
c              5 zta: double
c              6 zta: half
c     and generate the correct datain.bc and init.input file
c *****************************************************************

      integer imax,jmax,kmax,ndim
      integer ngrid,i,j,k,m,n,ii,rotate
      integer, dimension(:), allocatable:: idim,jdim,kdim,
     &     i1dim,j1dim,k1dim
      double precision, allocatable, dimension(:,:,:,:):: x,x1
      double precision rl,alpha,pi,x01,x02
      character*50 filename
      character*7 pref

      integer:: bin_gen = 0
      integer option

      write(*,*) 'Mesh name ?'
      read(*,*) filename     
      open(4,file=filename,status='old',form='formatted')
      open(7,file='mesh_chk.x',form='formatted')
      open(8,file='mesh_new.x',form='unformatted')

      write(*,*) 'Write bin files?'
      read(*,*) bin_gen

c *** constant
      pi=4.d0*atan(1.)

      write(*,*)'please input the dimension (2/3):'
      read(*,*) ndim
      if (ndim.eq.2) then
         pref="bin_2d_"
      else
         pref="bin_3d_"
      end if

      read(4,*)ngrid

      option = 1
      write(*,*)"please input the option:"
      print*," 1 xie: double"
      print*," 2 xie: half"
      print*," 3 eta: double"
      print*," 4 eta: half"
      print*," 5 zta: double"
      print*," 6 zta: half"

      read(*,*) option
c
      allocate (idim(ngrid),jdim(ngrid),kdim(ngrid))
      allocate (i1dim(ngrid),j1dim(ngrid),k1dim(ngrid))
c
      write(*,*)'The total number of subzones is',ngrid
      read(4,*)(idim(n),jdim(n),kdim(n),n=1,ngrid)
            
      write(*,*) "write init.input"
      open(10,file='pre_init.input',form='formatted')
      write(10,*)20*ngrid, 0
      write(10,*)ngrid, 0

c
c *** compute the max values of the dimensions
c
      imax = 0
      jmax = 0
      kmax = 0
      do n=1,ngrid
        write(*,*)n,idim(n),jdim(n),kdim(n)
        if (imax.lt.idim(n)) imax = 3*idim(n)
        if (jmax.lt.jdim(n)) jmax = 3*jdim(n)
        if (kmax.lt.kdim(n)) kmax = 3*kdim(n)
      end do
c
      allocate (x(imax,jmax,kmax,3))
      allocate (x1(imax,jmax,kmax,3))

c
      write(7,*)ngrid
      write(8) ngrid

      do n=1,ngrid
        select case(option)
        case(1) ! 2*xie
           k1dim(n) = kdim(n) 
           j1dim(n) = jdim(n)
           i1dim(n) = 2*(idim(n)-1)+1
        case(2) ! 0.5*xie
           k1dim(n) = kdim(n)
           j1dim(n) = jdim(n)
           i1dim(n) = (idim(n)-1)/2+1 !il must be an even number
        case(3) ! 2*eta
           i1dim(n) = idim(n)
           k1dim(n) = kdim(n)
           j1dim(n) = 2*(jdim(n)-1)+1
        case(4) ! 0.5*eta
           i1dim(n) = idim(n)
           k1dim(n) = kdim(n)
           j1dim(n) = (jdim(n)-1)/2+1 !kl must be an even number
        case(5) !  2*zta
           i1dim(n) = idim(n)
           j1dim(n) = jdim(n)
           k1dim(n) = 2*(kdim(n)-1)+1
        case(6) ! 0.5*zta
           i1dim(n) = idim(n)
           j1dim(n) = jdim(n)
           k1dim(n) = (kdim(n)-1)/2+1 !kl must be an even number

        end select

        write(7,*)i1dim(n),j1dim(n),k1dim(n)

      enddo
cccccccccccccccccccccccccccccccccccccccccccccccc

      write(8)(i1dim(n),j1dim(n),k1dim(n),n=1,ngrid)

      do n=1,ngrid

        read(4,*)((((x(i,j,k,ii),i=1,idim(n)),
     $               j=1,jdim(n)),k=1,kdim(n)),ii=1,3)
        write(*,*)'The subzones ',n,' has been read'

cccccccccccccccccccccccccccccccc
        select case(option)
        case(1)
c     
           print*,'case1 double the xie dimension'
           print*, i1dim(n),j1dim(n), k1dim(n)

        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              do m=1,ndim

                if (mod(i, 2) .gt. 0) then 
                x1(i,j,k,m) = x((i+1)/2,j,k,m) !odd
                else
                x1(i,j,k,m) = (x(i/2,j,k,m)+x(i/2+1,j,k,m))*0.5 !even
                endif
!                print*,i,j,k,x1(i,j,k,m)

              end do
            end do
          end do
        end do

      case(2)
         print*,'case2 half the xie dimension'
         print*, i1dim(n),j1dim(n), k1dim(n)

        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              do m=1,ndim

                x1(i,j,k,m) = x(i*2-1,j,k,m) !odd
!                print*,i,j,k,x1(i,j,k,m)

              end do
            end do
          end do
        end do

        case(3)
c     
           print*,'case3 double the eta dimension'
           print*, i1dim(n), j1dim(n), k1dim(n)

        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              do m=1,ndim

                if (mod(j, 2) .gt. 0) then 
                x1(i,j,k,m) = x(i,(j+1)/2,k,m) !odd
                else
                x1(i,j,k,m) = (x(i,j/2,k,m)+x(i, j/2+1, k,m))*0.5 !even
                endif
!                print*,i,j,k,x1(i,j,k,m)
              end do
            end do
          end do
        end do

      case(4)
         print*,'case4 half the eta dimension'

         print*, i1dim(n),j1dim(n), k1dim(n)

        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              do m=1,ndim

                x1(i,j,k,m) = x(i,j*2-1,k,m) !odd
!                print*,i,j,k,x1(i,j,k,m)

              end do
            end do
          end do
        end do

        case(5)
c     
           print*,'case5 double the zta dimension'
           print*, i1dim(n), j1dim(n), k1dim(n)

        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              do m=1,ndim

                if (mod(k, 2) .gt. 0) then 
                x1(i,j,k,m) = x(i,j,(k+1)/2,m) !odd
                else
                x1(i,j,k,m) = (x(i,j,k/2,m)+x(i,j,k/2+1,m))*0.5 !even
                endif
!                print*,i,j,k,x1(i,j,k,m)

              end do
            end do
          end do
        end do

      case(6)
         print*,'case 6 half the zta dimension'

         print*, i1dim(n),j1dim(n), k1dim(n)

        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              do m=1,ndim

                x1(i,j,k,m) = x(i,j,k*2-1,m) !odd
!                print*,i,j,k,x1(i,j,k,m)

              end do
            end do
          end do
        end do

      end select
cccccccccccccccccccccccccccccccc

c *** write mesh information of current block
      if(bin_gen.eq.1) then
        write(filename, '(a7,i3.3,".dat")') pref,n
        print*,"block  ",n,"  is written"
        open(9, file=filename,form='unformatted')
        write(9) i1dim(n)-1,j1dim(n)-1,k1dim(n)-1
        do k = 1, k1dim(n)
          do j = 1, j1dim(n)
            do i = 1, i1dim(n)
              write(9) (x1(i,j,k,m),m=1,ndim)
            end do
          end do
        end do
        close(9)
      endif
c
        write(7,*)((((x1(i,j,k,ii),i=1,i1dim(n)),
     $               j=1,j1dim(n)),k=1,k1dim(n)),ii=1,3)
        write(8)((((x1(i,j,k,ii),i=1,i1dim(n)),
     $               j=1,j1dim(n)),k=1,k1dim(n)),ii=1,3)

      end do
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      do n=1,ngrid
         write(10,*)i1dim(n)-1,j1dim(n)-1,k1dim(n)-1
      end do
      write(10,*)0
      close(10)

      close(4)
      close(7)
      close(8)
      deallocate(idim,jdim,kdim,x,x1,i1dim,j1dim,k1dim) 
c
      end
