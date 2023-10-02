! This program will perform the full pivoting of the matrix
! This is buit to incorporate in Gauss Elimination
program Full_pivot

      implicit none

      real,allocatable,dimension(:,:) :: a
      real :: ipointer
      real :: temp
      integer :: n
      integer :: i,j,k
      integer :: alloc_stat,dalloc_stat
      integer :: ipi,ipj 
      character(len = 80) :: alloc_msg,dalloc_msg

      print*,"Enter the total number of unknowns"
      read(*,*) n

      !************************** ALLOCATION ****************************

      allocate(a(n,n+1), STAT=alloc_stat,ERRMSG=alloc_msg)

      alloc_if:if(alloc_stat==0)then
              print*,"Matrix is allocated"
      else alloc_if
              print*,"Matrix is not allocated"
              print*,alloc_msg
      end if alloc_if

      !************************ DATA INPUT ******************************

      100 format (A,I2,A,I2,A)
      inpi_do : do i=1, n
      inpj_do : do j=1, n+1

      write(*,100) "Enter a(",i,",",j,")"
      read(*,*) a(i,j)

      end do inpj_do
      end do inpi_do

      print*,"============= INPUT ==============="
      101 format(4f8.3)!............................................Format
      write(*,101) transpose(a)
      print*,"==================================="

      !*********************** CHECKING HIGHEST ELEMENT *******************

      checkk_do: do k=1,n-1
      
      ipointer = a(k,k)

      print*,"ipointer = ",ipointer

      checki_do: do i=k,n
      checkj_do: do j=k,n

      if(ipointer< a(i,j))then
              ipointer=a(i,j)
              ipi=i
              ipj=j
      end if

      end do checkj_do
      end do checki_do

      print*,"ipointer = ",ipointer

      if_1: if(ipointer/=a(k,k)) then

      excc_do: do i=1,n
      temp = a(i,k)
      a(i,k)=a(i,ipj)
      a(i,ipj)=temp
      end do excc_do
      print*,"===== Intermediate OUTPUT (column exchange)=============="
      write(*,101) transpose(a)
      print*,"========================================================="

      excr_do: do i=1,n+1
      temp = a(k,i)
      a(k,i)=a(ipi,i)
      a(ipi,i)=temp
      end do excr_do
      print*,"====== Intermediate OUTPUT (Row exchange) ==============="
      write(*,101) transpose(a)
      print*,"==================================="

      end if if_1

      end do checkk_do

      print*," "
      print*," "
      print*,"============= OUTPUT ==============="
      write(*,101) transpose(a)
      print*,"==================================="


      ! ************************ DEALLOCATE *******************************

      deallocate(a, STAT=dalloc_stat,ERRMSG=dalloc_msg)

      dalloc_if:if(dalloc_stat==0)then
              print*,"Matrix is deallocated"
      else dalloc_if
              print*,"Matrix is not deallocated"
              print*,dalloc_msg
      end if dalloc_if


      end program Full_pivot
