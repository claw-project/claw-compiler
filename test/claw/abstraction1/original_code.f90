PROGRAM test_abstraction1
  REAL, DIMENSION(0:60) :: q, t
  q(0) = 1.0
  CALL compute_column(q, t)
  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction1

SUBROUTINE compute_column(q, t)
  IMPLICIT NONE
  REAL, DIMENSION(0:60), INTENT(INOUT) :: q, t
  REAL :: c
  INTEGER :: k
  INTEGER :: kend = 60

  ! Define a dimension that will be added to the variable in data clause
  ! Apply the parallelization transformation and add new dimension to the
  ! variables declared in the data clause
  !$claw define dimension proma(1,NPROMA) &
  !$claw parallelize data(q,t) over (proma,:)

  c = 5.345
  DO k = 1, kend
    t(k) = c * k
    q(k) = q(k - 1)  + t(k) * c
  END DO
  q(kend) = q(kend) * c
END SUBROUTINE
