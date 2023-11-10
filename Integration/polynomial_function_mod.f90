module polynomial_function

      implicit none

      save

      contains

               real function fn(a,b,c,d)

                       integer,intent(in) :: a
                       real,intent(in) :: b
                       real,dimension(a) ::c,d
                       real :: p
                       
                       integer :: i

                       fn = 0

                       do i=1,a
                       p=c(i)*(b**d(i))
                       fn=fn+p

                       end do

                     

                      end function fn

              end module polynomial_function
