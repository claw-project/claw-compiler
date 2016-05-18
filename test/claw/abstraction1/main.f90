! Test the CLAW abstraction model with one additional dimension.
PROGRAM test_abstraction1
  USE column_module, ONLY: compute_column

  REAL, DIMENSION(20,60) :: q, t  ! Fields as declared in the whole model
  INTEGER :: nproma, nz           ! Size of array fields
  INTEGER :: p                    ! Loop index

  nproma = 20
  kend = 60

#ifdef _CLAW
  !CALL compute_column_claw(nz, q, t, xend, yend)
#else
  DO p = 1, nproma
    CALL compute_column(nz, q(p,:), t(p,:))
  END DO
#endif

  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction1
