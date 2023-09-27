module Gauss_Eli
      
      implicit none

      real,allocatable,dimension(:,:) :: a
      real,allocatable,dimension(:) :: x
      real :: temp,u,v,templ
      integer,allocatable,dimension(:) :: label
      integer ::n
      integer ::alstat,dalstat,status_2,labstat,dlabstat
      integer :: i,j,k
      integer :: ipointer,ipi,ipj
      character(len=80) :: emsg,labemsg

      save

      contains


      !************************* ALLOCATION **************************        

              subroutine albox()

                      implicit none


                      print*,"Enter the total number of unknowns"
                      read(*,*) n

                      allocate(a(n,n+1),STAT=alstat, ERRMSG=emsg)
                      
                      if(alstat==0)then

                              print*,"Box is allocated "
                      else
                              print*,"Box is not allocated"
                              print*,emsg
                      end if

                      allocate(label(n),STAT=labstat,ERRMSG=labemsg)

                      if_1:if(labstat==0)then

                              print*,"Label box is allocated"

                      else if_1

                              print*,"Label box is not allocated"

                      end if if_1


                      return                      

              end subroutine albox

              

        ! ******************* DEALLOCATION ********************************

              subroutine dalbox()

                      implicit none

                     deallocate(a,STAT=dalstat)

                     if(dalstat==0) then 

                             print*,"Box is deallocated"
                     else
                             print*,"Box is not deallocated"
                     end if

                     deallocate(label,STAT=dlabstat)

                     if_1: if (dlabstat==0)then
                             print*,"Label box is deallocated"
                     else if_1
                             print*,"Label box is not deallocated"
                     end if if_1

                     return 

              end subroutine dalbox

         ! ****************** Label box *******************************

         subroutine labbox()

                 implicit none

                 print*,"Box labelled"

         do i=1,n
          
         label(i) = i

         end do 

         end subroutine labbox


         ! ****************** INPUT INTAKE *****************************
           
           subroutine input()

                   implicit none

           i_do:do i=1,n
           j_do:do j=1,n+1


           write(*,100)"Enter the value of a(",i,",",j,")"
           read(*,*) a(i,j)

          100 format(A,i0,A,i0,A)

           end do j_do
           end do i_do

           return
           end subroutine input

         ! ********************** DISPLAY OUTPUT ***************************
         subroutine display()

                 implicit none


                 write(*,101) transpose(a)

                 101 format(4f8.3)

                 return

                 end subroutine display


         ! ********************* Pivoting ********************************

        subroutine full_pivot()       

              implicit none  

              checkk_do: do k=1,n-1

              ipointer = a(k,k)


              checki_do: do i=k,n
              checkj_do: do j=k,n

              if(ipointer< a(i,j))then
                       ipointer=a(i,j)
                       ipi=i
                       ipj=j
              end if

              end do checkj_do
              end do checki_do


              if_1: if(ipointer/=a(k,k)) then

                      templ = label(k)
                      label(k)=label(ipj)
                      label(ipj)=templ

              excc_do: do i=1,n
              temp = a(i,k)
              a(i,k)=a(i,ipj)
              a(i,ipj)=temp
              end do excc_do
              print*," "
              print*," "
              print*,"===== Intermediate OUTPUT (column exchange)======"
              call display()
              print*,"================================================="

              excr_do: do i=1,n+1
              temp = a(k,i)
              a(k,i)=a(ipi,i)
              a(ipi,i)=temp
              end do excr_do
              print*," "
              print*," "
              print*,"====== Intermediate OUTPUT (Row exchange) ======="
              call display()
              print*,"================================================="

              end if if_1

              end do checkk_do 

              print*," "
              print*," "
              print*,"============= Pivoted Matrix ==============="
              call display()
              print*,"============================================"






      end subroutine full_pivot


      !************************ BACK SUBSTITUTION *******************************

      subroutine back_sub()

              implicit none


              cal_k: do k=1,n
              cal_out:do i=k+1,n
              u=a(i,k)/a(k,k)

              cal_in: do j=1,n+1

              a(i,j) = a(i,j) - u*a(k,j)

              end do cal_in
              print*,"========================="
              print*,"    Intermediate step"
              print*,"    Gauss Elimination"
              print*,"========================="
              call display()
              print*,"========================="
              print*,"           End "
              print*,"========================="
              end do cal_out
              end do cal_k

              allocate(x(n),STAT=status_2)

              stat2:if(status_2==0)then
                      print*,"x is allocated"
              else stat2
                      print*,"x is not allocated"
              end if stat2

              bs_cal_out:do i=n,1,-1
              v=0
              bs_cal_in:do j=n,i+1,-1
              cyc:if(i>=j) then
                    cycle bs_cal_in
              end if cyc
              v= a(i,j)*x(j)+v
              end do bs_cal_in
                  x(i)=(a(i,n+1)-v)/a(i,i)
              end do bs_cal_out

              end subroutine back_sub

       !***************** Label Display ****************************

       subroutine labdis()

               implicit none

               do i=1,n-1


               temp = x(i)

               x(i)=x(label(i))

               x(label(i))=temp

               end do

              print*,"========================="
              print*,"     Final   Result"
              print*,"========================="
              result_1:do i=1,n
              write(*,103) "x(",i,")",x(i)
              103 format (A,i0,A,f8.3)
              end do result_1
              print*,"========================="
              print*,"      End Final result"
              print*,"========================="

                

               end subroutine labdis


      end module Gauss_Eli
