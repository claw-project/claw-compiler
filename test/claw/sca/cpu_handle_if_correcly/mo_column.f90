!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS
  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t, z)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Vertical dimension size
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as single column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as single column only
    REAL, INTENT(INOUT)   :: z(:) ! Field declared as single column only
    REAL :: tmp, tmp2   ! Temporary variable
    INTEGER :: k  ! Loop index over the verical dimension

    ! CLAW definition
    ! Define horizontal dimension that will be added to the auto-detected
    ! variables.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw sca

    DO k = 1, nz
      IF (t(k) > 0.) THEN
        IF(k < 10) THEN
          tmp = tmp + q(k)
          q(k) = q(k) / t(k)
        END IF
      ELSE
        q(k) = q(k) * z(k)
      END IF
      tmp2 = tmp + q(k)
      z(k) = z(k) * tmp
    END DO
  END SUBROUTINE compute_column
END MODULE mo_column
