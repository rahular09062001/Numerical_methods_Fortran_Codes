program Lagrange_Interpolation 

      use LInt_mod
      implicit none

      print*,"Enter the number of known points"
      read(*,*) n

      call box_al()

      call dat_inp()

      print*,"Enter the point you want check for interpolation"
      read(*,*)x1

      call dis()

      call Lag_Int()

      call dbox_al()

      end program Lagrange_Interpolation
