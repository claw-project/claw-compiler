!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the loop-hoist directive with fusion clause
!

program loop_hoist6
  call clawloop
end program loop_hoist6

subroutine clawloop
  IMPLICIT NONE
  INTEGER :: i
  INTEGER :: j
  INTEGER :: k
  INTEGER :: iend = 2
  INTEGER :: jend = 4
  INTEGER :: kend = 2

  DO i=0,iend
    !$claw loop-hoist(j,k) fusion group(j)
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

    !$claw loop-fusion group(j)
    DO j=0,jend
      PRINT*,'ALONE J LOOP'
    END DO

  END DO
end subroutine clawloop
