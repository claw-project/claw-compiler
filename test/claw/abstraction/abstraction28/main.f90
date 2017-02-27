! Test the CLAW abstraction model with one additional dimension.
PROGRAM test_abstraction28
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

  !$omp target data map(to: q,t) map(from: q, t)

  !$claw parallelize forward
  DO p = 1, nproma
    CALL compute_column(nz, q(p,:), t(p,:))
  END DO

  !$omp end target data

  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction28
