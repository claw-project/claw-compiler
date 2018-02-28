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

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: z(:) ! Field declared as one column only
    REAL :: tmp ! Temporary variable
    INTEGER :: k                  ! Loop index

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw parallelize

    DO k = 1, nz
      IF (t(k) > 0.) THEN
        IF(k < 10) THEN
          tmp = tmp + q(k)
          q(k) = q(k) / t(k)
        END IF
      ELSE
        q(k) = q(k) * z(k)
      END IF
      z(k) = z(k) * tmp
    END DO
  END SUBROUTINE compute_column
END MODULE mo_column
