!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the to-loop directive
!

PROGRAM VECTOR_LOOP
  CALL claw
  CALL claw_transformed
END

! Before the transformation
SUBROUTINE claw
  INTEGER :: j
  INTEGER, DIMENSION(0:10) :: vec1

  DO j = 0, 10
    vec1(j) = j
  END DO

  !$claw expand parallel
  vec1(:) = vec1(:) + 10

  PRINT*,vec1
END SUBROUTINE claw


! Example of the transformation for development purpose
SUBROUTINE claw_transformed
  INTEGER :: i = 10
  INTEGER, DIMENSION(0:10) :: vec1
  INTEGER :: claw_induc1 ! induction variable is declared

  DO j = 0, i
    vec1(j) = j
  END DO

  ! Do stmt is inserted
  DO claw_induc1=0,i
    ! index range are replaced with arrayIndex
    vec1(claw_induc1) = vec1(claw_induc1) + 10
  END DO

  PRINT*,vec1
END SUBROUTINE claw_transformed
