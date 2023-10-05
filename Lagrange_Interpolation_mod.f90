module LInt_mod

      implicit none
      save

      integer :: n
      integer :: i,j
      integer :: xstat,fxstat,dxstat,dfxstat
      real,allocatable,dimension(:) :: x,fx
      real :: x1
      real :: li,pli
      character(len=80) :: xmsg,fxmsg,dxmsg,dfxmsg

      contains

      !*********************** Display *************************  

      subroutine dis()

        implicit none

        print*,"/////////////////////////////////"
        print*,"       Point and Function        "
        print*,"/////////////////////////////////"

        write(*,101) "x ","f(x)"
        101 format(A10,3X,A10)

        100 format(f10.5,3x,f10.5)
        do i=1,n

        write(*,100)x(i),fx(i)

        end do

       return  

      end subroutine dis      



      !************************ Allocation ******************************

      subroutine box_al()

              implicit none

      allocate(x(n),STAT=xstat,ERRMSG=xmsg)
      allocate(fx(n),STAT=fxstat,ERRMSG=fxmsg)

      if(xstat==0 .and. fxstat==0)then

              print*,"The boxes for x and f(x) are allocated"

      else
              print*,"all boxes are not allocated"
              print*,xmsg
              print*,fxmsg

      end if

      return 

              end subroutine box_al

      !*********************** Deallocation ***************************

      subroutine dbox_al()

              implicit none

      deallocate(x,STAT=dxstat,ERRMSG=dxmsg)
      deallocate(fx,STAT=dfxstat,ERRMSG=dfxmsg)

      if(dxstat==0 .and. dfxstat==0)then

              print*,"The boxes for x and f(x) are not deallocated"

      else
              print*,"all boxes are not allocated"
              print*,dxmsg
              print*,dfxmsg

      end if

      return

              end subroutine dbox_al

      !*************************** Data Input ************************

      subroutine dat_inp()

              implicit none

              do i=1,n

              write(*,102)i,i
              102 format("Enter x",i0," and f(x",i0,")")
              read(*,*)x(i),fx(i)

              end do

              return 

              end subroutine dat_inp

       !********************* Lagrange Interpolation *********************

       subroutine Lag_Int()

               implicit none

               li = 0

               i_do:do i=1,n

               pli = 1
               j_do:do j=1,n

               if(i/=j)then

                      pli = pli*(x1 - x(j))/(x(i)-x(j))

               end if

               end do j_do

               li = li + (fx(i)*pli)
               end do i_do

               print*,"----------------------"
               write(*,*)"f(",x1,") = ",li
               print*,"-----------------------"



               end subroutine Lag_Int

      end module LInt_mod
