!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the CLAW abstraction model with one additional dimension.
!

PROGRAM test_abstraction1
  USE mo_column, ONLY: compute_point
  INTEGER, PARAMETER :: nproma = 20
  REAL, DIMENSION(nproma) :: q, t, w  ! Fields as declared in the whole model
  INTEGER :: i, k

  do i = 1, nproma
    t(i) = 0.5 * i
  end do

  !$claw sca forward create update
  q = compute_point(t, w = w)

  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction1
