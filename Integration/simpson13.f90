program simpson_13

        use polynomial_function
        implicit none

        integer :: n_term
        real :: val,fval
        integer :: intv
        real,allocatable,dimension(:) :: col,expo
        integer :: istat_c,istat_e
        integer :: i
        real :: h
        real :: result_sum = 0
        real :: a,b,c

        print*,"+-----------------------------------+"
        print*,"|        Simpson 1/3 Method         |"
        print*,"+-----------------------------------+"
        print*,""
        print*,"+-----------------------------------+"
        print*,"|This program assumes that you have |"
        print*,"|a simple polynomial function of the|"
        print*,"|form as below.                     |"
        print*,"|ax^n + bx^(n-1)+ ... + cx + d = 0  |"
        print*,"+-----------------------------------+"

        write(*,*)"Enter the total number of terms in the polynomial"
        read(*,*) n_term

        allocate(col(n_term),STAT=istat_c)
        allocate(expo(n_term),STAT=istat_e)

        if(istat_c ==0 .and. istat_e == 0)then

        !*************************** Data_input ************************************

                input_do : do i=1,n_term

                write(*,*)"Enter the coefficent of term No.",i
                read(*,*)col(i)
                write(*,*)"Enter the exponential of term No.",i
                read(*,*)expo(i)

                end do input_do

        !************************** Calculation ***********************************

                write(*,*)"Enter intial and final value of interval"
                read(*,*)val,fval
                write(*,*)"Enter the total number sub interval (should be even)"
                read(*,*)intv

                cal_if: if(mod(intv,2)==0)then

                        h=(fval-val)/intv

                        cal_do: do i=1,int(intv/2)

                        a = fn(n_term,val,col,expo)
                        b = fn(n_term,(val+h),col,expo)
                        c = fn(n_term,(val+(2*h)),col,expo)
                        result_sum =result_sum + ((h/3)*(a+(4*b)+c))

                        val = val + (2*h)

                        end do cal_do

                        write(*,*)"Integration result = ",result_sum



                else cal_if

                        write(*,*)"The number you enterd is not even"

                        end if cal_if


        end if

        deallocate(col)
        deallocate(expo)


      end program simpson_13
