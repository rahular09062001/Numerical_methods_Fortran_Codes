module trap_mod

        implicit none

        save

        contains
                real function fn(a,b,c,d)

                        integer,intent(in) ::a  
                        real,intent(in) :: b
                        real,dimension(a)  ::  c,d

                        integer :: i
                        real :: p

                        fn = 0

                        do  i=1,a

                        p=c(i)*(b**d(i))
                        fn=fn+p

                        end do

                        end function fn

        end  module  trap_mod
