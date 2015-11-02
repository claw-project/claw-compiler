! Simple program to test the loop-interchange directive

PROGRAM LOOP_INTERCHANGE
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i, j, k

  DO i=1,4
    DO j=1,3
      DO k=1,2
        PRINT *, 'Iteration i=',i,', j=',j,', k=',k
      END DO
    END DO
  END DO
END
