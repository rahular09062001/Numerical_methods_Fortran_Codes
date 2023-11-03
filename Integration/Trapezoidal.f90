program trapezoidal_method

        use trap_mod
        implicit  none

        integer :: n_term,i,n_c
        real,allocatable,dimension(:) ::  cl,ex
        integer :: istat_1,istat_2
        real :: val,fval,h,valh
        integer  ::  interval
        real  :: s=0

        write(*,*)"+----------------------------------------+"
        write(*,*)"|         TRAPEZOIDAL METHOD             |"
        write(*,*)"+----------------------------------------+"
        print*,""
        print*,""

        write(*,*)"+----------------------------------------+"
        write(*,*)"| This program assumes that you have a   |"
        write(*,*)"|polynomial function of the form as below|"
        write(*,*)"|  ax^n + b+x^(n-1) + ....+ cx + d =  0  |"
        write(*,*)"+----------------------------------------+"
        print*,"Enter the number of  terms in the  polynomial"
        read(*,*)n_term

        allocate(cl(n_term),STAT=istat_1)
        allocate(ex(n_term),STAT=istat_2)

        if(istat_1==0  .and. istat_2==0)then

                do i=1,n_term

                write(*,*)"The coefficent of term no.",i
                read(*,*)cl(i)
                write(*,*)"The exponential of term no.",i
                read(*,*)ex(i) 

                end do

                write(*,*)cl
                write(*,*)ex

                write(*,*)"Enter the initial value  of  x"
                read(*,*)val

                write(*,*)"Enter the final  value of x"
                read(*,*)fval


                write(*,*)"Enter the total no. of intervals in b/n"
                read(*,*)interval

                h=(fval - val)/real(interval)

                write(*,*) "h = ",h

                do2:do i=1,interval

                valh = val + h
                s=s+(h/2.0)*(fn(n_term,val,cl,ex)+fn(n_term,valh,cl,ex))
                val = valh

                end do do2

                write(*,*)"Integrated Value  = ",s


        end if

        deallocate(cl)
        deallocate(ex)

        end program trapezoidal_method 
