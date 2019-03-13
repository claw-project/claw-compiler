!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
program openacc_continuation
  implicit none

  integer :: i
  real, dimension(100)  :: a
  real, dimension(100)  :: bvariable, cvariable, dvariable, evariable, &
                           fvariable, gvariable, hvariable, ivariable, &
                           jvariable, kvariable, lvariable

  !$claw acc data copyin(a) &
  !$claw copyin(bvariable, cvariable, dvariable, evariable, fvariable) &
  !$claw copyin(gvariable, hvariable, ivariable, jvariable) &
  !$claw copyin(kvariable, lvariable)
  !$claw acc parallel
  !$claw acc loop gang vector
  do i=1, 100
    a(i) = bvariable(i)*2.0
  end do
  !$claw acc end parallel
  !$claw acc end data
end program openacc_continuation
