!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

MODULE mo_column
  IMPLICIT NONE
CONTAINS

  SUBROUTINE compute(nz, q, t, s)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nz   ! Size of the array field
    REAL, INTENT(INOUT) :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT) :: q(:) ! Field declared as one column only
    REAL, INTENT(INOUT) :: s    ! Scalar in SCA but array in model data

    !$claw sca forward
    CALL compute_column(nz, q, t, s)

  END SUBROUTINE compute


  ! Compute only one column
  SUBROUTINE compute_column(nz, q, t, s)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nz   ! Size of the array field
    REAL, INTENT(INOUT) :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT) :: q(:) ! Field declared as one column only
    REAL, INTENT(INOUT) :: s    ! Scalar in SCA but array in model data
    REAL, DIMENSION(:), ALLOCATABLE :: y
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw sca scalar(s)

    IF(.NOT. ALLOCATED(y)) ALLOCATE(y(nz))

    ! claw parallel region should start from here

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      y(k) = t(k) + s
      q(k) = q(k - 1)  + t(k) * c + y(k)
    END DO
    q(nz) = q(nz) * c

    ! claw parallel region should end here

    IF(ALLOCATED(y)) DEALLOCATE(y)
  END SUBROUTINE compute_column
END MODULE mo_column
