! This program will calculate the root by using bisection method
! Here I have used the function f(x)= x^2 + x -7
! The function can be changed 
! *********************************8********Main Program******************************************************************
program Bisection_method

      implicit none

      real(kind=16) :: x0,x1,x2
      real(kind=16) :: fun
      real(kind=16),parameter :: er = 0.0000000000000000000000000000000000000001
      real(kind=16) :: rle ! relative error
    
      print*,"Enter the Interval you want to check"
      read(*,*) x0,x1

      if(fun(x0)*fun(x1)<0) then

        print*,"The value is accepted"
        print*,"f(x0)",fun(x0)
        print*,"f(x1)",fun(x1)

        loop_1: do

        rle = x2
       print*,"x0 =",x0,"x1 = ",x1, "f(x1) =",fun(x1),"f(x0) = ",fun(x0) 
        x2 = (x0+x1)/2

      if_1:if(fun(x0)*fun(x2)<0) then

              x1 = x2

      else if_1

              x0=x2

      end if if_1
      print*,"x2 = ",x2,"f(x2) = ",fun(x2)      

      if_2 : if(abs(rle-x2) <= er) then

               exit loop_1
       end if if_2
       end do loop_1

       print*,"The root value in this range is = ",x2

      else
              print*,"The value is not accepted"

      end if


      end program Bisection_method

! **********************************************Function*************************************************************

     real(kind=16) function fun(a)

      implicit none

      real(kind=16),intent(in) :: a

      fun = a**2 + a - 7

      return 

      end function fun
