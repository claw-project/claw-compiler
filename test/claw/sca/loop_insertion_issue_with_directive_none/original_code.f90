!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test case for issue #598
module sca49
  contains
  subroutine sub1()
    integer :: i, j
    integer, parameter :: ny = 10, nx = 10

    !$claw define dimension k(1:n_term) &
    !$claw sca
    do j=1, ny
      do i=1, nx
      end do
    end do
  end subroutine sub1
end module