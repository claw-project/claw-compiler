! Test the CLAW abstraction model with two additional dimensions.
PROGRAM test_abstraction2
  USE mo_column, ONLY: compute_column

  REAL, DIMENSION(10,10,60) :: q, t ! fields as declared in the whole model
  INTEGER :: nx, ny, nz             ! Size of array fields
  INTEGER :: i,j                    ! Loop indices

  nx = 10
  ny = 10
  nz = 60

#ifdef _CLAW
  !CALL compute_column_claw(kend, q, t, xend, yend)
#else
  DO i = 1, nx
    DO j = 1, ny
      CALL compute_column(kend, q(i,j,:), t(i,j,:))
    END DO
  END DO
#endif

  PRINT*,SUM(q)
  PRINT*,SUM(t)
END PROGRAM test_abstraction2
