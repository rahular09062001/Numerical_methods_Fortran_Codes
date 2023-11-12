program RK_4

        implicit none

        real :: f
        real :: x0,xn,y0,y1
        real :: h
        integer :: intv
        real  :: k1,k2,k3,k4
        integer :: i

        print*,"+-------------------------------------+"
        print*,"|      RUNGE-KUTTA Fourth Order       |"
        print*,"+-------------------------------------+"
        print*,""
        print*,"+-------------------------------------+"
        print*,"|The function can be changed in the   |"
        print*,"|in the function section of code.     |"
        print*,"|By default the function is taken as  |"
        print*,"|   f(a,b) = (b**2)+(a**2)                 |"
        print*,"+-------------------------------------+"

        write(*,*)"Enter the intial and final of x"
        read(*,*)x0,xn
        write(*,*)"Enter the total number of interval (integer value)"
        read(*,*)intv
        write(*,*)"Enter the value of y at intial value  of x"
        read(*,*)y0

        h = (xn-x0)/intv

        do i=1,intv

        k1 = h*f(x0,y0)
        k2 = h*f((x0 + (h/2.0)),(y0 + (k1/2.0)))
        k3 = h*f((x0 + (h/2.0)),(y0 + (k2/2.0)))
        k4 = h*f((x0 + h),(y0 + k3))
        
        y1 = y0 + ((k1 + (2.0*k2) + (2.0*k3) + k4)/6.0)
        
        x0 = x0 + h
        y0 = y1

        end do 

        write(*,*)"Result = ",y1

        end program RK_4

        real function f(a,b)

                implicit none

                real,intent(in) :: a,b

                f = (b**2)+(a**2)

                return

                end function f
