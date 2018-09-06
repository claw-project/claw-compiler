!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
program openacc_continuation
  implicit none

  integer :: i
  real, dimension(100)  :: a
  real, dimension(100)  :: b

  !$claw acc data copyin(a) &
  !$claw acc      present(b)
  !$claw acc parallel
  !$claw acc loop gang vector
  do i=1, 100
    a(i) = b(i)*2.0
  end do
  !$claw acc end parallel
  !$claw acc end data
end program openacc_continuation
