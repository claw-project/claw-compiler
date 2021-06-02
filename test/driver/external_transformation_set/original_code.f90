!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

module mod1
  contains
    subroutine sub1()
    end subroutine sub1

    subroutine sub2()
      call sub1()
    end subroutine sub2
end module mod1
