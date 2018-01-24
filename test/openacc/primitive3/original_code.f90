!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM test_primitive

  INTEGER :: i

  !$claw acc data present(q,&
  !$claw acc& p,&
  !$claw acc& h)&
  !$claw acc& create(pt)

  !$claw acc parallel
  DO i = 1, 10
    PRINT*,i
  END DO
  !$claw acc end parallel

  !$claw omp do
  DO i = 1, 10
    PRINT*,i
  END DO
  !$claw omp end do

END PROGRAM test_primitive
