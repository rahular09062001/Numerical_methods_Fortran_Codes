program Runge_kutta_second_order

        implicit  none

        real :: f
        real :: x0,xn,y0,y1
        real :: h
        real :: k1,k2
        integer :: intv
        integer :: i

        print*,"+----------------------------------+"
        print*,"|     RUNGE-KUTTA Second-order     |"
        print*,"+----------------------------------+"

        print*,""
        print*,"+----------------------------------+"
        print*,"|The function can be changed in the|"
        print*,"|function section of code.         |"
        print*,"|By default the function is set as |"
        print*,"| f =(b**2)-(a**2)                 |"
        print*,"+----------------------------------+"

        write(*,*)"Enter the intial and final value of x"
        read(*,*)x0,xn
        write(*,*)"Enter the total number of intervals between intial and final value  of x"
        read(*,*)intv
        write(*,*)"Enter the value of y at intial value of x"
        read(*,*)y0

        h=(xn-x0)/intv

        do i=1,intv
        
        k1 = h*f(x0,y0)
        k2 = h*f((x0+h),(y0+k1))
        y1 = y0 + ((k1+k2)/2.0)

        x0 = x0+h
        y0 = y1

        end do 

        print*,"Result = ",y1



        end program Runge_kutta_second_order

        real function f(a,b) !******************************FUNCTION

                implicit none

                real,intent(in) :: a,b

                f =(b**2)-(a**2)
                
                return 
                end function f
