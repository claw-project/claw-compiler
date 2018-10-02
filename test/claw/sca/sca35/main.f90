!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

PROGRAM test_column_conditional
  USE mo_column, ONLY: compute_column
  REAL, DIMENSION(5,10) :: q, t, z  ! Fields as declared in the whole model
  INTEGER :: nproma, nz             ! Size of array fields
  INTEGER :: p                      ! Loop index

  nproma = 5
  nz = 10

  DO p = 1, nproma
    q(p, 1:nz) = 6.0
    z(p, 1:nz) = 5.0
    t(p, 1:6) = 2.0
    t(p, 6:nz) = 0.0
  END DO

  !$claw sca forward create update
  DO p = 1, nproma
    CALL compute_column(nz, q(p,:), t(p,:), z(p,:))
  END DO

  PRINT*, (q(1, i), i=1,10)
END PROGRAM test_column_conditional
