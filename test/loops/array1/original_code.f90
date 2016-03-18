! Simple program to test the to-loop directive

PROGRAM VECTOR_LOOP
  CALL claw
END

! Before the transformation
SUBROUTINE claw
  INTEGER :: j
  INTEGER :: i = 10
  INTEGER, DIMENSION(0:9) :: vec1

  DO j = 0, i
  		vec1(j) = j
  END DO

  !$claw array-transform
  vec1(0:i) = vec1(0:i) + 10;

  PRINT*,vec1
END SUBROUTINE claw
