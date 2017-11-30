!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the remove directive
!

PROGRAM remove1
  CALL dummy
END PROGRAM remove1

SUBROUTINE dummy
  INTEGER :: k,i
  INTEGER :: kend = 5, iend = 10
  DO k=1, kend
    DO i=1, iend
      ! loop #1 body here
    END DO

    !$claw remove
    IF (k > 1) THEN
      PRINT*, k
    END IF

    DO i=1, iend
      ! loop #2 body here
    END DO
  END DO

END SUBROUTINE dummy
