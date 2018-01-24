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

  !$omp parallel
  DO k=1,10
    !$omp do
    !$claw loop-fusion
    DO i=1,10
      PRINT *, 'First loop body:',i
    END DO
    !$omp end do

    !$omp do
    !$claw loop-fusion
    DO i=1,10
      PRINT *, 'Second loop body:',i
    END DO
    !$omp end do
  END DO
  !$omp end parallel
END
