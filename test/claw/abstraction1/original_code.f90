! Test the CLAW abstraction model with one additional dimension.
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

    ! CLAW definition

    ! Define one dimension that will be added to the variables defined in the
    ! data clause.
    ! Apply the parallelization transformation on this subroutine.

    !$claw define dimension proma(1,nproma) &
    !$claw parallelize data(q,t) over (proma,:)

    c = 5.345
    DO k = 2, nz
      t(k) = c * k
      q(k) = q(k - 1)  + t(k) * c
    END DO
    q(nz) = q(nz) * c
  END SUBROUTINE compute_column
END MODULE mo_column

PROGRAM test_abstraction1
  USE mo_column, ONLY: compute_column
  REAL, DIMENSION(20,60) :: q, t  ! Fields as declared in the whole model
  INTEGER :: nproma, nz           ! Size of array fields
  INTEGER :: p                    ! Loop index

  nproma = 20
  nz = 60

  DO p = 1, nproma
    q(p,1) = 0.0
    t(p,1) = 0.0
  END DO

  !$acc data copyin(q,t) copyout(q,t)

#ifdef _CLAW
  CALL compute_column(nz, q, t, nproma)
#else
  DO p = 1, nproma
    CALL compute_column(nz, q(p,:), t(p,:))
  END DO
#endif

  !$acc end data

  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction1
