MODULE mo_column
  IMPLICIT NONE
CONTAINS

  SUBROUTINE compute_all(nz, q, t)
    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    REAL, TARGET :: z(1:nz)
    INTEGER :: k
    REAL, POINTER :: zp (:)

    DO k=1,nz
      z(k) = t(k) + q(k)
    END DO

    zp => z

    !$claw parallelize forward
    CALL compute_column(nz, q, t)


  END SUBROUTINE compute_all

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
    !$claw parallelize &
    !$claw data(q) over(proma,:) &
    !$claw data(t) over(:,proma)

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      q(k) = t(k - 1)  + t(k) * c
    END DO
    q(nz) = q(nz) * c

  END SUBROUTINE compute_column
END MODULE mo_column
