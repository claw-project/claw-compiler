! Simple program to test the loop-fusion directive with collapse clause

PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i
  INTEGER :: j

  !$claw loop-fusion collapse(2)
  DO i=1,5
    DO j=1,4
      DO k=1,2
        PRINT *, 'First loop body:',i,j
      END DO
    END DO
  END DO

  !$claw loop-fusion collapse(2)
  DO i=1,5
    DO j=1,4
      DO k=1,2
        PRINT *, 'Second loop body:',i,j
      END DO
    END DO
  END DO
END
