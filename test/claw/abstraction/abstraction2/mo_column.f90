MODULE mo_column
  IMPLICIT NONE
CONTAINS
  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient
    REAL :: d                     ! Intermediate varibale

    ! CLAW definition

    ! Define two dimensions that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension i(1:nx) &
    !$claw define dimension j(1:ny) &
    !$claw parallelize

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      d = t(k) + c
      q(k) = q(k - 1)  + t(k) * c + d
    END DO
    q(nz) = q(nz) * c
  END SUBROUTINE compute_column
END MODULE mo_column
