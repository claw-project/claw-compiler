!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

! Simple program to test the array notation to do loop directive

PROGRAM VECTOR_LOOP
  CALL claw
END

! Before the transformation
SUBROUTINE claw
  INTEGER :: i = 10
  INTEGER, DIMENSION(0:10,0:10) :: vec1
  INTEGER, DIMENSION(0:10,0:10) :: vec2

  vec1(0:i,0:i) = 0;
  vec2(0:i,0:i) = 100;

  !$claw expand target(cpu)
  vec1(0:i,0:i) = vec2(0:i,0:i) + 10

  !$claw expand target(gpu)
  vec1(0:i,0:i) = vec2(0:i,0:i) + 10

  PRINT*,vec1
  PRINT*,vec2
END SUBROUTINE claw
