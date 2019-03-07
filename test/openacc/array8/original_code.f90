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
  INTEGER, DIMENSION(0:10) :: vec1
  INTEGER, DIMENSION(0:10) :: vec2
  INTEGER, DIMENSION(0:10) :: vec3
  INTEGER, DIMENSION(0:20) :: vec4
  INTEGER, DIMENSION(0:20) :: vec5

  vec1(:) = 0;
  vec2(:) = 100;
  vec4(:) = 10;
  vec5(:) = 11;

  !$claw expand parallel 
  vec1(:) = vec2(:) + 10
  vec2(:) = vec1(:) + 10

  vec4(:) = vec5(:) + 1
  vec4(:) = vec4(:) + 1
  !$claw end expand

  vec3(:) = vec1(:) + vec2(:)


  PRINT*,vec1
  PRINT*,vec2
END SUBROUTINE claw_test
