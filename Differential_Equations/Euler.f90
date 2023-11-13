program euler

      implicit none

      real :: x0,xn
      real :: y0,yn
      real :: h
      real :: f
      integer :: intv
      integer :: i

      write(*,*)"Enter the intial value of x"
      read(*,*)x0
      write(*,*)"Enter the value of x for which y is to determined "
      read(*,*)xn
      write(*,*)"Enter the intial value of y"
      read(*,*)y0
      write(*,*)"Enter the interval between the values of x"
      read(*,*)intv

      h = (xn-x0)/intv
      print*,"h = ",h

      do i=1,intv

      
      print*,"x0 = ",x0
      print*,"y0 = ",y0

      yn = y0 + (h*f(x0,y0))

      x0 = x0 + h
      y0 = yn
      
      print*,"xn = ",x0 
      print*,"yn = ",yn
      print*,"--------------------------"

      end do 

      write(*,*)"Result = ",yn

      end program euler

      real function f(a,b)

              implicit none

              real,intent(in) :: a
              real,intent(in) :: b

              f= (a**2)+(b**2)

              return 

              end function f
