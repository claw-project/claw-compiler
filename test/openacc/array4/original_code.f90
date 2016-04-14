! Simple program to test the array notation to do loop directive
! test block transformation of the array-transform directive

PROGRAM array4_test
  CALL claw_test
END

! Before the transformation
SUBROUTINE claw_test
  INTEGER, DIMENSION(0:10) :: vec1
  INTEGER, DIMENSION(0:10) :: vec2
  INTEGER, DIMENSION(0:10) :: vec3

  vec1(:) = 0;
  vec2(:) = 100;

  !$claw array-transform
  vec1(:) = vec2(:) + 10
  vec2(:) = vec1(:) + 10
  !$claw end array-transform

  vec3(:) = vec1(:) + vec2(:)


  PRINT*,vec1
  PRINT*,vec2
END SUBROUTINE claw_test
