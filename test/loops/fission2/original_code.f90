!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the loop-fusion directive
!

PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop
  INTEGER :: i

  DO i=1,10
    PRINT *, 'First loop body:',i

    !$claw loop-fission

    PRINT *, 'Second loop body:',i

    !$claw loop-fission

    PRINT *, 'Third loop body:',i

    !$claw loop-fission

    PRINT *, 'Fourth loop body:',i

  END DO

END
