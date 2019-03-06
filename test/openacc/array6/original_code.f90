!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

! Simple program to test the array notation to do loop directive
! Test with var for lower/upper bound

PROGRAM VECTOR_LOOP
  CALL claw
END

! Before the transformation
SUBROUTINE claw
  INTEGER :: upper = 10
  INTEGER :: lower = 0
  INTEGER, DIMENSION(0:10) :: vec1
  INTEGER, DIMENSION(0:10) :: vec2

  vec1 = 0;
  vec2 = 100;

  !$claw expand
  vec1(lower:upper) = vec2(lower:upper) + 10

  !$claw expand
  vec2(lower + 1:upper) = vec2(1) + 10

  PRINT*,vec1
  PRINT*,vec2
END SUBROUTINE claw
