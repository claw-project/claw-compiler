!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS
  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    REAL, DIMENSION(nz) :: y
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw sca

    c = 5.345
234 c = c*c
    DO k = 2, nz
      t(k) = c * k
      q(k) = q(k - 1)  + t(k) * c
    END DO

    IF (q(nz) <= 0.0) GOTO 234

    q(nz) = q(nz) * c
  END SUBROUTINE compute_column
END MODULE mo_column
