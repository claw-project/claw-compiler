!
! This file is released under terms of BSD license
! See LICENSE file for more information
!

! Simple program to test the array notation to do loop directive
! test block transformation of the expand directive

PROGRAM array4_test
  CALL claw_test
END

! Before the transformation
SUBROUTINE claw_test
  INTEGER, DIMENSION(10) :: vec1
  INTEGER, DIMENSION(10) :: vec2
  INTEGER, DIMENSION(10) :: vec3

  vec1(:) = 0;
  vec2(:) = 100;

  PRINT*,vec1
  PRINT*,vec2

  !$claw expand
  vec1(:) = vec2(:) + 10
  vec2(:) = vec2(:) + 10
  !$claw end expand

  PRINT*,vec1
  PRINT*,vec2
  vec3(:) = vec1(:) + vec2(:)


  PRINT*,SUM(vec1)
  PRINT*,SUM(vec2)
END SUBROUTINE claw_test
