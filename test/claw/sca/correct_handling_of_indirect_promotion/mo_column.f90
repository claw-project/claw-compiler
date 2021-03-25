!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS

  SUBROUTINE compute(nz, q, t, z)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: z    ! Field declared as scalar

    !$claw sca forward
    CALL compute_column(nz, q, t, z)

  END SUBROUTINE compute


  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t, z)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: z    ! Field declared as scalar
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient
    REAL :: p

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw sca data(t,q,z) over(proma,:)

    c = 5.345

    p = c**2.0

    DO k = 2, nz
      t(k) = c * k
      p = t(k) + 1.0
      q(k) = q(k - 1) + t(k) * c
      IF(p > 2.0) THEN
        q(k) = q(k - 1) + t(k) * c * 2.0
      END IF
    END DO
    q(nz) = q(nz) * c
  END SUBROUTINE compute_column
END MODULE mo_column
