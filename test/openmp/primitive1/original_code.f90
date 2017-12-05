!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM test_primitive

  INTEGER :: i

  !$claw acc parallel
  DO i = 1, 10
    PRINT*,i
  END DO
  !$claw acc end parallel

  !$claw omp do
  DO i = 1, 10
    PRINT*,i
  END DO
  !$claw   omp end do

END PROGRAM test_primitive
