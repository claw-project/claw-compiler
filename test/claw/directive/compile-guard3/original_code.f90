!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the CLAW compile guard OpenACC
!

PROGRAM compile_guard_test
  CALL dummy_subroutine
END PROGRAM

SUBROUTINE dummy_subroutine
  INTEGER :: i
  !$acc claw-guard
  DO i=1,10
    PRINT *, 'First loop body:',i
  END DO

  DO i=1,10
    PRINT *, 'Second loop body:',i
  END DO
END SUBROUTINE
