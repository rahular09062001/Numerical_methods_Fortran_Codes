program Gauss_Elimination

      use Gauss_Eli
      implicit none


      call albox()

      call labbox()

      call input()
      
      print*,"======= Input ========="
      call display()
      print*,"======================="

      call full_pivot()

      call back_sub()

      call labdis()

      call dalbox()

      end program Gauss_Elimination
