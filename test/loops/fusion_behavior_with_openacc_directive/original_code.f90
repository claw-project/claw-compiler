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
  INTEGER :: i, k

  !$acc parallel
  !$acc loop seq
  DO k=1,10
    !$acc loop
    !$claw loop-fusion
    DO i=1,10
      PRINT *, 'First loop body:',i
    END DO

    !$acc loop
    !$claw loop-fusion
    DO i=1,10
      PRINT *, 'Second loop body:',i
    END DO
  END DO
  !$acc end parallel

END
