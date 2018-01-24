!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the loop-fusion directive in different block level
! the transformation should not occur as dirctive's parent block are different
!

PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop
  IMPLICIT NONE
  INTEGER :: i
  INTEGER :: j
  INTEGER :: k

  !$claw loop-fusion group(i)
  DO i=1,5
    DO j=1,4
      DO k=1,2
        PRINT *, 'First loop body:',i,j
      END DO
    END DO
  END DO

  DO j=1,4
    !$claw loop-fusion group(i)
    DO i=1,5
      DO k=1,2
        PRINT *, 'Second loop body:',i,j
      END DO
    END DO
  END DO
END
