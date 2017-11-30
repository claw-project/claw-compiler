!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the loop-interchange directive
!

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

  !$claw loop-interchange target(cpu)
  DO i=1,10
    DO j=1,2
      PRINT *, 'Iteration ',i,',',j
    END DO
  END DO
END
