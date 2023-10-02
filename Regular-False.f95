! This program uses Regular-False method to calculate the root
! The functions can be changed in the function section
! The minimum error can be changed in the error part of main program

!**********************    MAIN  PROGRAM    **********************
program Regular_False

      implicit none

      real :: x0,x1,x2=0 
      real :: fun
      real :: rle 
      real,parameter :: err = 0.00001 !................ ERROR
      integer :: i=0

      print*,"Enter the value of range"
      read(*,*) x0,x1

      if_1:if(fun(x0)*fun(x1)<0)then

              print*,"The range is accepted"

              loop_1:do

              i=i+1
              
              rle = x2

              x2 = x0 - (fun(x0)/(fun(x1)-fun(x0)))*(x1-x0)

              if_2:if((fun(x2)*fun(x0))<0)then

                      x1 = x2

              else if_2

                      x0 = x2

              end if if_2

              if(abs(rle - x2) < err)exit loop_1

              end do loop_1

              print*,"Number of iterations = ",i
              print*,"The root is = ",x2

      else if_1

              print*,"The range is not accepted"
              print*,"f(x0) = ",fun(x0)," f(x1) = ",fun(x1)
              print*,"f(x0) x f(x1) = ",fun(x0)*fun(x1)

      end if if_1

      end program Regular_False


      ! **********************   FUNCTION   ***********************


              real function fun(a)

                      implicit none

                      real,intent(in) :: a

                      fun = a**3 + 2*a - 2

                      return 

                      end function fun
