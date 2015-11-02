! Simple program to test the loop-fusion directive

PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i
  !$claw loop-fusion
  DO i=1,10
    PRINT *, 'First loop body:',i
  END DO

  !$claw loop-fusion
  DO i=1,10
    PRINT *, 'Second loop body:',i
  END DO

  !$claw loop-fusion
  DO i=1,10
    PRINT *, 'Third loop body:',i
  END DO

END
