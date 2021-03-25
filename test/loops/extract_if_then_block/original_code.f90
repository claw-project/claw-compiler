!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the if-extract directive
!

program ifextract1
  integer :: i
  logical :: test

  test = .TRUE.

  !$claw if-extract
  DO i = 1, 10
    IF (test) THEN
      PRINT *, 'First loop body:', i
    END IF
  END DO

end program ifextract1
