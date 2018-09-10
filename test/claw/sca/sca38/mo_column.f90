!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Test the CLAW abstraction model with one additional dimension.
!
MODULE mod1
  IMPLICIT NONE
CONTAINS
  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t, z, flag)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Vertical dimension size
    REAL, INTENT(INOUT)   :: t(nz) ! Field declared as single column only
    REAL, INTENT(INOUT)   :: q(nz) ! Field declared as single column only
    REAL, INTENT(INOUT)   :: z(nz) ! Field declared as single column only
    LOGICAL, INTENT(IN)   :: flag  ! Flag to trigger a certain process
    REAL :: tmp, tmp2   ! Temporary variable
    INTEGER :: k  ! Loop index over the verical dimension

    ! CLAW definition of parallelization across the horizontal dimension.

    !$claw define dimension proma(1:nproma) &
    !$claw parallelize

    DO k=1, nz, 1
       q(k) = q(k) / t(k)
    END DO

    IF (flag) THEN
      DO k=1, nz, 1
        IF (z(k) < tmp) THEN
           z(k) = z(k) * tmp2
        END IF
      END DO
   END IF

  END SUBROUTINE compute_column
END MODULE mod1
