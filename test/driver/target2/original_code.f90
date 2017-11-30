!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the loop-fusion directive with macro
!

PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i
  !$claw loop-fusion
  DO i=1,10
    PRINT *, 'First loop body:',i
  END DO

#ifdef _OPENMP
  !$claw loop-fusion
  DO i=1,10
    PRINT *, 'Second loop body:',i
  END DO
#endif

  !$claw loop-fusion
  DO i=1,10
    PRINT *, 'Third loop body:',i
  END DO

END
