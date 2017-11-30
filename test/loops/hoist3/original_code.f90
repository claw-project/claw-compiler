!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
!
! Simple program to test the loop-fusion directive with collapse clause
!

PROGRAM LOOP_FUSION
  CALL clawloop
END

SUBROUTINE clawloop
  IMPLICIT NONE
  INTEGER :: i
  INTEGER :: j
  INTEGER :: k
  INTEGER :: iend = 2
  INTEGER :: jend = 4
  INTEGER :: kend = 2

  DO i=0,iend
    !$claw loop-hoist(j,k) target(cpu)
    IF (i == 0) THEN
      PRINT*, 'First iteration of i',i,'/',j,'/',k
    END IF
    DO j=0,jend
      DO k=0,kend
        PRINT *, 'First loop body:',i,'/',j,'/',k
      END DO
    END DO

    DO j=2,jend
      DO k=0,kend
        PRINT *, 'Second loop body:',i,'/',j,'/',k
      END DO
    END DO
    !$claw end loop-hoist

    !$claw loop-hoist(j,k) target(gpu)
    IF (i == 0) THEN
      PRINT*, 'GPU First iteration of i',i,'/',j,'/',k
    END IF
    DO j=0,jend
      DO k=0,kend
        PRINT *, 'GPU First loop body:',i,'/',j,'/',k
      END DO
    END DO

    DO j=2,jend
      DO k=0,kend
        PRINT *, 'GPU Second loop body:',i,'/',j,'/',k
      END DO
    END DO
    !$claw end loop-hoist
  END DO
END
