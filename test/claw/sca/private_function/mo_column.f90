!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: compute_column_public

CONTAINS

  SUBROUTINE compute_column_public(nz, q, t)
    IMPLICIT NONE
    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only

    !$claw sca forward
    CALL compute_column(nz, q, t)

  END SUBROUTINE compute_column_public


  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t)
    IMPLICIT NONE
    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw sca

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      q(k) = q(k - 1)  + t(k) * c
    END DO
    q(nz) = q(nz) * c
  END SUBROUTINE compute_column


END MODULE mo_column
