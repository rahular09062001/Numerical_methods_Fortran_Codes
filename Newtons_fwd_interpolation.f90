program Newtons_fwd_Interpolation

      implicit none

      integer :: n,i,j
      integer :: alstat_1,alstat_2
      integer :: dalstat_1,dalstat_2
      real :: h,u,xg,y_1
      real,allocatable,dimension(:) :: x,y,dy
      character(len=80) :: almsg_1,almsg_2
      character(len=80) :: dalmsg_1,dalmsg_2
      real :: sum_1,prod=1

      print*,"Enter the number of points available"
      read(*,*)n

!***************************ALLOCATION*******************************
      
      allocate(x(n),STAT=alstat_1,ERRMSG=almsg_1)
      allocate(y(n),STAT=alstat_1,ERRMSG=almsg_2)

      alloc_if:if(alstat_1==0 .and. alstat_2==0)then

!************************** DATA INPUT *****************************

              print*,"Enter the equidistatnt x point values"
      do i=1,n

      write(*,101)"Enter the value of x point No.",i
      read(*,*)x(i)

      write(*,101)"Enter the vlaue of y point No.",i
      read(*,*)y(i)

      101 format(A,I0)

      end do 

      end if alloc_if

      h = x(2) - x(1)

      print*,"Enter the value of x for y have to be calculated"
      read(*,*)xg

      u=(xg-x(1))/h

      sum_1=y(1)

!************************* CALCULATION *****************************

      do i=1,n-1

      inner: do j=1,n-i

      y(j)=y(j+1)-y(j)

      end do inner

      prod=prod*((u-(i-1))/i)
      sum_1=sum_1 + (prod*y(1))

      end do 

      write(*,103)"Value of y are x = ",xg," is : ",sum_1
      103 format(A,f10.5,A,f10.5)

!*********************** DEALLOCATION ******************************

      deallocate(x)
      deallocate(y)


      end program Newtons_fwd_Interpolation
