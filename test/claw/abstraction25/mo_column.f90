MODULE mo_column
  IMPLICIT NONE
CONTAINS

  ! Compute only one column
  FUNCTION compute_column(nz, q, t) RESULT(r)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(1:nz) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient
    INTEGER :: r                  ! Function return value

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw parallelize data(t,q) over(:,proma)

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      q(k) = q(k - 1)  + t(k) * c
    END DO
    q(nz) = q(nz) * c
  END FUNCTION compute_column

  SUBROUTINE compute(nz, q)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL                  :: t(1:nz) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    INTEGER :: result

    !$claw parallelize forward
    result = compute_column(nz, q, t)

  END SUBROUTINE compute

END MODULE mo_column
