! Simple program to test the loop-interchange directive

PROGRAM LOOP_INTERCHANGE
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i, j, k, a, b, c, x, y, z
  !$claw loop-interchange new-order(k,i,j)
  DO i=1,4
    DO j=1,3
      DO k=1,2
        PRINT *, 'Iteration i=',i,', j=',j,', k=',k
      END DO
    END DO
  END DO

  !$claw loop-interchange new-order(b,c,a)
  DO a=1,4
    DO b=1,3
      DO c=1,2
        PRINT *, 'Iteration i=',i,', j=',j,', k=',k
      END DO
    END DO
  END DO

  !$claw loop-interchange new-order(x,z,y)
  DO x=1,4
    DO y=1,3
      DO z=1,2
        PRINT *, 'Iteration i=',i,', j=',j,', k=',k
      END DO
    END DO
  END DO
END
