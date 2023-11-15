program ADBM

      implicit none

      real :: f,rk4
      real :: xn
      real,dimension(4) :: x,y,fxy 
      real :: h
      real :: yp,yc
      integer :: i

      print*,"+--------------------------------------------+"
      print*,"|        Predictor-Corrector Methods         |"
      print*,"|      ADAMS-BASHFORTH-MOULTON method        |"
      print*,"+--------------------------------------------+"
      print*,""
      print*,"+--------------------------------------------+"
      print*,"|     By default the function is set as      |"
      print*,"|             f(x,y) = 2y- y^2               |"
      print*,"|It can be changed in the function section   |"
      print*,"|code.                                       |"
      print*,"+--------------------------------------------+"
      print*,""
      

      write(*,*)"Enter the intial value of x and y"
      read(*,*)x(1),y(1)
      write(*,*)"Enter the value of x for calculation of y "
      read(*,*)xn

      h=(xn-x(1))/4.0
      

      do_1:do i = 2,4

      x(i)=x(i-1)+h
      y(i)=rk4(x(i-1),y(i-1),h)

      end do do_1

      do_2:do i=1,4

      fxy(i)=f(x(i),y(i))

      end do do_2

      yp = y(4) + (h*((-9*fxy(1))+(37*fxy(2))-(59*fxy(3))+(55*fxy(4)))/24.0)

      print*,"Yp = ",yp

      yc = y(4) +(h*((fxy(2))-(5*fxy(3))+(19*fxy(4))+(9*f(xn,yp)))/24.0)

      print*,"Yc = ",yc

      print*,"Result = ",yc

      end program ADBM

      real function f(a,b)

              implicit none

              real,intent(in) :: a
              real,intent(in) :: b

              f = (2*b)-(b**2)

              return 

              end function f

      real function rk4(a,b,h)

              implicit none

              real,intent(in) :: a,b,h
              real :: f
              real :: k1,k2,k3,k4
              integer :: i

              k1=h*f(a,b)
              k2=h*f((a+(h/2.0)),(b+(k1/2.0)))
              k3=h*f((a+(h/2.0)),(b+(k2/2.0)))
              k4=h*f((a+h),(b+k3))

              rk4 = b + ((k1 + (2.0*k2) + (2.0*k3) + k4)/6.0)

              return

              end function rk4
