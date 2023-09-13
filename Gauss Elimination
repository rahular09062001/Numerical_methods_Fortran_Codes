! This program uses Gauss Elimination method to solve the simultaneous equations

program Gauss_Elimination

        ! WARNING: change the format when needed

      implicit none

      integer:: n1,n2
      real,allocatable,dimension(:,:) :: a
      real :: u,v
      integer :: i,j,k
      integer :: status_1,status_2
      real,allocatable,dimension(:)::x

      print*,"WARNING: Change the format 102 according to "
      print*,"the number of column and row"

      print*,"Enter the total number of equations"
      read(*,*)n1
      n2=n1+1

      allocate (a(n1,n2), STAT=status_1)

      alloc_if:if(status_1==0) then
      print*,"The rows and columns are allocated"
      else alloc_if
              print*,"The rows and columns are not allocated"
      end if alloc_if

      input_i: do i=1,n1
      input_j: do j=1,n2

      write(*,101)"Enter the value of a(",i,",",j,")"
      101 format (A,I2,A,I2,A)
      read(*,*) a(i,j)

      end do input_j
      end do input_i

      print*,"========================="
      print*,"     The data input"
      print*,"========================="
      write(*,102)transpose(a)
      102 format (4f8.3) !...........................Format 102
      print*,"========================="
      print*,"    End of input data"
      print*,"========================="

      cal_k: do k=1,n2-1
      cal_out:do i=k+1,n1
      u=a(i,k)/a(k,k)
      print*,a(i,k)
      print*,a(k,k)
      print*,"k = ",u

      cal_in: do j=1,n2

      a(i,j) = a(i,j) - u*a(k,j)
      print*,a(i,j)
      
      end do cal_in 
      print*,"========================="
      print*,"    Intermediate step"
      print*,"========================="
      write(*,102)transpose(a)
      print*,"========================="
      print*,"           End "
      print*,"========================="
      end do cal_out
      end do cal_k


      print*,"========================="
      print*,"       The output 1"
      print*,"========================="
      write(*,102)transpose(a)
      print*,"========================="
      print*,"      End of output 1"
      print*,"========================="

      allocate(x(n1),STAT=status_2)

      stat2:if(status_2==0)then
              print*,"x is allocated"
      else stat2
              print*,"x is not allocated"
      end if stat2

      bs_cal_out:do i=n1,1,-1
      v=0
      bs_cal_in:do j=n1,i+1,-1
      cyc:if(i>=j) then 
              cycle bs_cal_in
      end if cyc
      v= a(i,j)*x(j)+v
      end do bs_cal_in
      x(i)=(a(i,n1+1)-v)/a(i,i)
      end do bs_cal_out

      print*,"========================="
      print*,"        Result"
      print*,"========================="
      result_1:do i=1,n1
      write(*,103) "x(",i,")",x(i)
      103 format (A,i0,A,f8.3)
      end do result_1
      print*,"========================="
      print*,"      End result"
      print*,"========================="

      deallocate(a,STAT=status_1)
      deallocate(x,STAT=status_2)

      end program Gauss_Elimination
