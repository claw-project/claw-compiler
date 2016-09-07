MODULE mo_column
  IMPLICIT NONE
CONTAINS



  ! Compute only one column
  FUNCTION compute_column(nz, q, t) result(res)
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    INTEGER :: k                  ! Loop index
    REAL :: c                     ! Coefficient
    REAL :: res(1:nz)

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1:nproma) &
    !$claw parallelize &
    !$claw data(q) over(proma,:) &
    !$claw data(t,res) over(:,proma)

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      q(k) = t(k - 1)  + t(k) * c
    END DO
    q(nz) = q(nz) * c

    res = t

  END FUNCTION compute_column

  SUBROUTINE compute_all(nz, q, t)
    INTEGER, INTENT(IN)   :: nz   ! Size of the array field
    REAL, INTENT(INOUT)   :: t(:) ! Field declared as one column only
    REAL, INTENT(INOUT)   :: q(:) ! Field declared as one column only
    REAL, TARGET :: z(1:nz)
    INTEGER :: k
    REAL, POINTER :: zp (:)
    REAL, TARGET :: res (1:nz)
    REAL, POINTER :: res_p (:)

    DO k=1,nz
      z(k) = t(k) + q(k)
    END DO

    zp => z

    !$claw parallelize forward
    res(:) = compute_column(nz, q, t)

    res_p => res


  END SUBROUTINE compute_all
END MODULE mo_column
