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
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: vec1
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: vec2

  ALLOCATE(vec1(10,20))
  ALLOCATE(vec2(10,20))

  vec1(:,:) = 0;
  vec2(:,:) = 100;

  !$claw expand
  vec1(:,:) = vec2(:,:) + 10
  vec2(:,:) = vec2(:,:) + 10
  !$claw end expand

  PRINT*,SUM(vec1)
  PRINT*,SUM(vec2)

  DEALLOCATE(vec1)
  DEALLOCATE(vec2)
END SUBROUTINE claw_test
