!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS
  ! Compute only one column
  SUBROUTINE compute_column(p1, nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    INTEGER, INTENT(IN)   :: p1
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient
    INTEGER, PARAMETER :: c0 = 8.0


    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw sca

    IF (p1 == c0) THEN
      DO k = 2, nz
        t(k) = c * k
        q(k) = q(k - 1)  + t(k) * c
      END DO
    END IF

    q(nz) = q(nz) * c
  END SUBROUTINE compute_column
END MODULE mo_column
