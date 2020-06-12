!
! This file is released under terms of BSD license
! See LICENSE file for more information
!
! Simple program to test the kcache directive
!

MODULE kcache_module
USE helper_module, ONLY: array1, array2, array3

contains

SUBROUTINE kcache()

  INTEGER :: i,j

  DO i = 1,10
    DO j = 1,20
      array1(i,j) = 1.0
      array2(i,j) = 2.0
      array3(i,j) = 3.0
    END DO
  END DO

  DO i = 1,10
    DO j = 2,20
      !$claw kcache data(array2, array3)
      !$claw kcache data(array1) offset(0,-1)
      array1(i,j) = array1(i,j-1) * 2.0
      array2(i,j) = array2(i,j) * 2.0 + array1(i,j-1)
      array3(i,j) = array3(i,j) * 2.0 + array1(i,j-1) + array2(i,j)
      array1(i,j-1) = array2(i,j)
    END DO
  END DO
  PRINT*, SUM(array1)
  PRINT*, SUM(array2)
  PRINT*, SUM(array3)
END SUBROUTINE kcache

END MODULE kcache_module
