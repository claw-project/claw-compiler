!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the remove directive
!

PROGRAM remove3
  CALL dummy
END PROGRAM remove3

SUBROUTINE dummy
  INTEGER :: k,i
  INTEGER :: kend = 5, iend = 10
  DO k=1, kend
    DO i=1, iend
      ! loop #1 body here
    END DO

    DO i=1, iend
      ! loop #2 body here
    END DO
  END DO

  !$claw remove
  CONTAINS
    SUBROUTINE dummy_inside

    END SUBROUTINE

END SUBROUTINE dummy
