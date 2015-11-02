! Simple program to test the loop-interchange directive

PROGRAM LOOP_INTERCHANGE
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i, j
  !$claw loop-interchange
  DO i=1,10
    DO j=1,2
      PRINT *, 'Iteration ',i,',',j
    END DO
  END DO
END
