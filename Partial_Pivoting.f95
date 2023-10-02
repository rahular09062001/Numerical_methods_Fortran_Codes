! This Program will partialy pivot the matrix

program partial_pivoting

      implicit none

      real,allocatable,dimension(:,:) :: a
      real :: ipointer, temp
      integer :: i,j,stat_1,stat_2,n,l
      integer :: ipj
      character(len = 80) :: err_msg

      print*,"Beware of Format "
      print*,"Check the format "

      print*,"Total Number of equations"
      read(*,*)n

      !================ Allocation ==============================

      allocate(a(n,n+1),STAT=stat_1,ERRMSG=err_msg)
      alloc_if: if(stat_1==0)then
              print*," a is allocated"
      else alloc_if
              print*," a is not allocated"
              print*,err_msg
      end if alloc_if

      !====================== Data Input ========================

      input_out: do i=1,n
      input_in: do j=1,n+1
      write(*,101) i,j
      101 format("Enter: a(",I0,",",I0,")")
      read(*,*) a(i,j)
      end do input_in
      end do input_out

      !========================== Data ==========================

      print*,"===================="
      print*,"        DATA"
      print*,"===================="
      write(*,102) transpose(a)
      102 format(4f9.3) !*************************************** Format
      print*,"===================="
      print*,"      End  DATA"
      print*,"===================="

      !====================== Pivoting ==========================

      Piv_out: do i=1,n-1
      ipointer = a(i,i)
      ipj = 0
      Piv_in:  do j=i,n


      piv_if:if(a(j,i)>ipointer)then
              ipointer = a(j,i)
              ipj=j
      end if piv_if


      end do Piv_in

      piv_1:if(a(i,i) /= ipointer)then

      piv:do l=1,n+1
      temp = a(i,l)
      a(i,l)=a(ipj,l)
      a(ipj,l)=temp
      end do piv
      print*,"===================="
      print*,"  intermediate pivot"
      print*,"===================="
      write(*,102) transpose(a)
      print*,"===================="
      print*,"    End int pivot"
      print*,"===================="

      end if piv_1

      end do Piv_out

      !================= Printing Pivoted Matrix =================
     
      print*,"===================="
      print*,"        pivot"
      print*,"===================="
      write(*,102) transpose(a)
      print*,"===================="
      print*,"    End pivot DATA"
      print*,"===================="


      !====================== Deallocation ======================
   
      deallocate(a,STAT=stat_2)
      dalloc_if:if(stat_2==0)then
              print*,"a is deallocated"
      else dalloc_if
              print*,"a is not deallocated"
      end if dalloc_if

      end program partial_pivoting
