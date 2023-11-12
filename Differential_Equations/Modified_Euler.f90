program Modified_Euler

        implicit none

        real :: x0,x1,xn
        real ::  y0,y1,y11
        real :: h
        integer :: intv
        integer ::i
        real :: f

        print*,"+--------------------------------------+"
        print*,"|        Modified Euler Method         |"
        print*,"+--------------------------------------+"
        print*,""
        write(*,*)"Enter the intial and final value of x"
        read(*,*)x0,xn
        write(*,*)"Enter the intial value of y"
        read(*,*)y0
        write(*,*)"Enter the interval betwen intial and final value of x"
        read(*,*)intv

        h=(xn-x0)/intv

        outer: do i=1,intv

        y1 = y0 + (h*f(x0,y0))
        x1= x0 + h

        inner: do 

        y11 = y0 + (h/2)*(f(x0,y0)+f(x1,y1))
        
        if(abs(y11-y1) <=0.00001)exit inner

        y1=y11
        end do inner

        x0 = x1
        y0 = y1

        end do outer

        print*,""
        print*,"Result = ",y1



        end program Modified_Euler

        real function f(a,b)

               implicit none

               real,intent(in) :: a,b

               f= (a**2) - b
               return
               end function f
