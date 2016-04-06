! Simple program to test the array notation to do loop directive
! arrays do not have any index range. 

PROGRAM VECTOR_LOOP
  CALL claw
END

! Before the transformation
SUBROUTINE claw
  INTEGER :: i = 10
  INTEGER, DIMENSION(0:10,0:10) :: vec1
  INTEGER, DIMENSION(0:10,0:10) :: vec2

  vec1 = 0;
  vec2 = 100;

  !$claw array-transform
  vec1 = vec2 + 10

  PRINT*,vec1
  PRINT*,vec2
END SUBROUTINE claw
