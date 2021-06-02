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
  SUBROUTINE compute_column(nz, q, t, z, flag, flag2)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Vertical dimension size
    REAL, INTENT(INOUT)   :: t(nz) ! Field declared as single column only
    REAL, INTENT(INOUT)   :: q(nz) ! Field declared as single column only
    REAL, INTENT(INOUT)   :: z(nz) ! Field declared as single column only
    LOGICAL, INTENT(IN)   :: flag, flag2  ! Flags for certain processes
    REAL :: tmp, tmp2, tmp3(nz, 5)   ! Temporary variable
    INTEGER :: k, j  ! Loop index over the verical dimension

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

    DO k=1, nz, 1
       IF (flag) THEN
          z(k) = z(k) + tmp
          DO j=1, 5
             IF (flag2) THEN
                z(k) = z(k) * tmp3(k, j)
             END IF
          END DO
       END IF
    END DO

  END SUBROUTINE compute_column
END MODULE mod1
