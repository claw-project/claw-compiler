!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the remove directive
!

PROGRAM remove4
  CALL dummy
END PROGRAM remove4

SUBROUTINE dummy
  INTEGER :: k,i,z
  INTEGER :: kend = 5, iend = 10
  DO k=1, kend
    DO i=1, iend
      ! loop #1 body here
    END DO

    !$claw remove target(gpu)
    PRINT*, k
    PRINT*, k+1
    !$claw end remove

    !$claw remove target(cpu)
    z = k + 1
    z = z**2
    !$claw end remove
  END DO

END SUBROUTINE dummy
