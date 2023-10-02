! This program uses Newton-Raphson method to calculate the root
! The one draw back is we have to calculate derrivative by oursef
! The functions can be changed in the functions section
! The derrivative of function should be changed in the derrivative section

! ************************* MAIN PROGRAM  *********************************
program Newton_Raphson

      implicit none

      real :: x,x1
      real :: fn,fn_1
      real,parameter :: er = 0.000001
      integer :: i=0

      print*,"Enter the point to intialize"
      read(*,*) x

      loop_1: do 

      i = i+1

      x1 = x - (fn(x)/fn_1(x))
      if(abs(x1-x)<er)exit loop_1
      x = x1

      end do loop_1

      print*,"The number of interations = ",i
      print*,"The root = ", x1
      
      end program Newton_Raphson

     
      !************* Function  ****************

      real function fn(x)

      implicit none

      real,intent(in) :: x
      
      fn = x**3 + x - 1 

      return 

      end function fn

      !************** Derrivative Function *****************

      real function fn_1(x)

              implicit none

              real,intent(in) :: x

              fn_1 = (3*x**2) + 1

              return  

              end function fn_1

